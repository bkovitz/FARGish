# Model.py -- The canvas-and-painters model

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod
from itertools import chain, product
from io import StringIO
from random import choice, choices, random
import sys
from operator import itemgetter

from Types import Addr, CanvasValue, CellContent, End, F, \
    Fizzle, FizzleValueNotFound, \
    Func, I, Index, Indices, is_cell_content, J, \
    MaybeIndex, Painter, SimpleFunc, SoupRef, \
    Start, Value, Variable, WorkingSoup, \
    addr_str, func_str, is_painter, painter_str
import Types
from Canvas import Canvas, Canvas1D
from Soup import Soup
from Subst import Subst, empty_subst, Plus
from Funcs import same, pred, succ, MakeRelativeIndirectPainter, \
    MakeBetweenPainter
from Addrs import DetAddr, DetAddrWithSubst, RelatedPair, SpecialAddr, \
    MatchContent
from Painters import DetPainter0, DetPainter, DetFunc
from Log import lo, trace, indent_log, set_log_level
from util import short, nf, Numeric


default_primitive_funcs: FrozenSet[Func] = frozenset([same, succ, pred])
default_initial_painters: List[Painter] = [
    (RelatedPair(I, J, F), WorkingSoup, (I, J, F)),
    ((I, J, SimpleFunc(F)), WorkingSoup, MakeRelativeIndirectPainter(I, J, F)),
    ((I, Plus(I, 2), F), WorkingSoup, MakeBetweenPainter(I, J, F))
]

@dataclass
class Model:
    lts: Soup = field(default_factory=lambda: Soup.make_from(
        default_initial_painters
    ))
    ws: Soup = field(default_factory=lambda: Soup())
    canvas: Canvas1D = field(
        default_factory=lambda: Canvas1D.make_from('     ')
    )

    primitive_funcs: FrozenSet = default_primitive_funcs
    t: int = 0   # current timestep
    suppressions: Dict[DetPainter0, float] = field(default_factory=dict)
    dps_run: Dict[int, DetPainter] = field(default_factory=dict)
        # DetPainters run: a record of all the DetPainters that this model
        # has run, and in what timestep.
    auto_annotate: bool = True
        # Should the Model automatically annotate the first and last canvas
        # calls with Start and End?

    @classmethod
    def canvas_from(cls, s: str) -> Model:
        return cls(canvas=Canvas1D.make_from(s))

    @classmethod
    def make_from(cls, *args, auto_annotate: bool=True, **kwargs) -> Model:
        match len(args):
            case 0:
                pass
            case 1:
                if isinstance(args[0], str):
                    canvas = Canvas1D.make_from(
                        args[0],
                        auto_annotate=auto_annotate
                    )
                    kwargs['canvas'] = canvas
            case _:
                raise ValueError(
                    f'Can only pass a string to Model.make_from(); got {repr(args)}'
                )
        return cls(**kwargs)

    def set_canvas(self, s: str) -> None:
        self.canvas = Canvas1D.make_from(s)

    def contents_at(self, addr: Addr) -> Value:
        #TODO Look up a painter by addr?
        return self.canvas[addr]

    def paint(self, a: Index, x: CellContent) -> None:
        self.canvas[a] = x

    def absorb(self, s: str, timesteps: int=20):
        with indent_log('ABSORB', repr(s)):
            self.set_canvas(s)
            # TODO Set a mode where painters get penalized for painting the
            # wrong things
            # Run a little while, let some painters develop
            for t in range(timesteps):
                self.do_timestep()
                lo(3, self.state_str())
            # Save the abstract painters to the lts
            for p in self.ws:
                if self.is_absorbable(p):
                    self.lts.add(p)

    def is_absorbable(self, painter: Painter) -> bool:
        match painter:
            case (_, SoupRef(), _):
                return True
            case (str(), _, _):
                return True
            case _:
                return False

    def regen_from(self, s: str, nsteps: int=40) -> None:
        '''Regenerates canvas starting from 's'.'''
        with indent_log(2, 'REGENERATE from', repr(s)):
            self.ws.clear()
            with indent_log(3, 'LONG-TERM SOUP'):
                lo(3, self.lts.state_str())
            self.set_canvas(s)
            self.t = 0
            for t in range(nsteps):
                self.do_timestep()
                lo(3, self.state_str())

    def do_timestep(self) -> None:
        self.t += 1
        self.ws.decay()
        self.decay_suppressions()
        lo(2, f't={self.t}')
        dp = self.choose_detpainter(self.soups())
        try:
            self.run_detpainter(dp)
        except Fizzle as exc:
            lo(1, 'FIZZLE', exc)
        self.suppress(dp.as_painter())

    def choose_detpainter(self, soup: Soup) -> DetPainter:
        det_painters = list(chain.from_iterable(
            self.painter_to_detpainters(p)  #, soup.clarity(p))
                for p in soup
        ))
        weights = [
            self.detpainter_to_probability_weight(dp)
                for dp in det_painters
        ]
        with indent_log(4, 'DETPAINTERS'):
            if det_painters:
                for w, dp in sorted(
                    zip(weights, det_painters), key=itemgetter(0)
                ):
                    lo(4, nf(w), dp)
#                for k in range(len(det_painters)):
#                    lo(4, nf(weights[k]), det_painters[k])
            else:
                lo(4, 'No det_painters.')

        ii = choices(range(len(det_painters)), weights)[0]
        dp = det_painters[ii]
        lo(4, 'CHOSE DETPAINTER', dp, '  ', nf(weights[ii]))

        return dp

    def detpainter_to_probability_weight(self, dp: DetPainter) -> Numeric:
        return (
            self.source_weight(dp.source)
            *
            self.target_weight(dp.target)
            *
            dp.prob_weight
        )

    def suppress(self, dp0: DetPainter0) -> None:
        if dp0 in self.suppressions:
            self.suppressions[dp0] *= 0.5
        else:
            self.suppressions[dp0] = 0.1

    def suppression(self, dp0: DetPainter0) -> float:
        return self.suppressions.get(dp0, 1.0)

    def decay_suppressions(self) -> None:
        new_suppressions: Dict[Painter, float] = {}
        for dp0, sup in self.suppressions.items():
            new_sup = sup * 1.1
            if new_sup < 1.0:
                new_suppressions[dp0] = new_sup
        self.suppressions = new_suppressions

    def source_weight(self, a: DetAddr) -> Numeric:
        match a:
            case int():
                return self.canvas.clarity(a) / self.canvas.MAX_CLARITY
            case p if is_painter(p):
                return 1.0  # TODO: find out painter "clarity"
            case Indices(elems):
                return sum(
                    self.source_weight(elem)
                        for elem in elems
                            if isinstance(elem, int) #HACK
                )
        assert False, "source_weight(): should not go past 'match' stmt"

    def target_weight(self, a: DetAddr) -> Numeric:
        match a:
            case int():
                return 1.0 - self.canvas.clarity(a) / self.canvas.MAX_CLARITY
            case SoupRef():
                return 1.0
            case p if is_painter(p):
                return 1.0  # TODO: find out painter "clarity"
        assert False, "target_weight(): should not go past 'match' stmt"

    def run_detpainter(
        self,
        dp: DetPainter
    ) -> None:
        with indent_log(2, 'RUN_DETPAINTER', str(dp)):
            self.dps_run[self.t] = dp
            v = self.apply_func(dp.subst, dp.func, self.contents_at(dp.source))
            match dp.target:
                case int():
                    self.canvas[dp.target] = v  # type: ignore[assignment]
                case Types.WorkingSoup:
                    match v:
                        case p if is_painter(p):
                            self.ws.add(p)
                        case x:
                            raise ValueError(f'run_detpainter: try to paint {x} (type {type(x)}) to the workspace.')
                #TODO Painting to long-term soup
                case _:
                    raise NotImplementedError(f"run_detpainter: can't paint to target; dp={dp}")

    def ldp(self) -> Optional[DetPainter]:
        '''The last DetPainter run (if any).'''
        return self.dps_run.get(self.t, None)

    def see_ldps(self) -> None:
        '''Prints all the DetPainters and their timesteps.'''
        for t in sorted(self.dps_run.keys()):
            print(f't={t}\n{self.dps_run[t]}')

    def OLDpainter_to_detpainters(self, p: Painter) -> Iterable[DetPainter]:
        lo('PAINTER', short(p))
        source, target, func = p
        det_sources = self.addr_to_detaddrs(empty_subst, I, source)
        det_sources = list(det_sources) #DEBUG
        lo('DETSRC', det_sources)
        det_targets = chain.from_iterable(
            self.addr_to_detaddrs(ds.subst, J, target)
                for ds in det_sources
        )
        det_targets = list(det_targets) #DEBUG
        lo('DETTARG', det_targets)
        det_funcs = chain.from_iterable(
            self.func_to_detfuncs(dt.subst, F, func)
                for dt in det_targets
        )
        det_funcs = list(det_funcs) #DEBUG
        lo('DETFUNCS', det_funcs)
        # NEED to pair a subst with each DetAddr
        # PROBLEM Cartesian product is wrong because some sources may be
        # pared away due to matching targets; similarly for the funcs.
        return (
            DetPainter(
                dt.subst,
                ds.addr,
                dt.addr,
                df,
                1.0,   # TODO prob_weight,
                p  # basis, "author"
            ) for ds, dt, df in product(det_sources, det_targets, det_funcs)
        )

    def painter_to_detpainters(self, p: Painter) -> Iterable[DetPainter]:
        with indent_log(5, 'PAINTER to DETPAINTERS', p):
            source, target, func = p

            det_sources = self.addr_to_detaddrs(empty_subst, I, source)
            det_sources = list(det_sources) #DEBUG
            #lo(8, 'DETSRC', det_sources)
            with indent_log(8, 'DET SOURCES'):
                for det_source in det_sources:
                    lo(8, det_source)

            source_target_pairs = (
                (ds, dt)
                    for ds in det_sources
                        for dt in self.addr_to_detaddrs(ds.subst, J, target)
            )
            source_target_pairs = list(source_target_pairs) #DEBUG
            #lo(8, 'STPAIRS', list(zip(*source_target_pairs)))
            #lo(8, 'DETTARG', source_target_pairs[1])
            with indent_log(8, 'DET SOURCES+TARGETS'):
                for source_target_pair in source_target_pairs:
                    lo(8, source_target_pair)

            triples = (
                (ds, dt, df)
                    for (ds, dt) in source_target_pairs
                        for df in self.func_to_detfuncs(dt.subst, F, func)
            )
            triples = list(triples) #DEBUG
            #lo(7, 'DETFUNCS', triples)  # we really want the 3rd "column"
            #lo(7, 'DETFUNCS', triples[2])
            with indent_log(7, 'DET TRIPLES'):
                for triple in triples:
                    lo(7, triple)

            for ds, dt, df in triples:
                yield DetPainter(
                    dt.subst,
                    ds.addr,
                    dt.addr,
                    df,
                    self.suppression((ds.addr, dt.addr, df)),
                        # TODO figure painter clarity into prob_weight?
                    p  # basis, "author"
                )

    # TODO Rename to addr_to_detaddrs_with_subst
    def addr_to_detaddrs(self, subst: Subst, var: Variable, addr: Addr) \
    -> Iterable[DetAddrWithSubst]:
        with indent_log(6,
            'ADDR to DETADDRS',
            addr,
            f'(in Subst: {subst[addr]})' if addr in subst else '(not in Subst)',
            subst
        ):
            match addr:
                case int():
                    yield DetAddrWithSubst(subst.unify(var, addr), addr)
                case _ if is_cell_content(addr):
                    yield from (
                        DetAddrWithSubst(subst.unify(var, index), index)
                            for index in self.canvas.all_matching(addr)
                    )
                case MatchContent():
                    yield from (
                        DetAddrWithSubst(subst.unify(var, index), index)
                            for index in self.canvas.all_matching(addr.content)
                    )
                case Variable():
                    if addr in subst:
                        yield from (
                            self.addr_to_detaddrs(subst, var, subst.simplify(addr))
                        )
                    else:
                        yield from (
                            DetAddrWithSubst(
                                subst.unify(var, index).unify(addr, index),
                                index
                            )
                                for index in self.canvas.all_addrs()
                        )
                case Plus():
                    match subst.as_index(addr):
                        case None:
                            return
                        case int() as index:
                            yield DetAddrWithSubst(subst.unify(var, index), index)
                        case _:
                            raise NotImplementedError(f"Can't match Plus that simplifies to {addr}, {type(addr)}")
                case SpecialAddr():
                    #yield from addr.to_detaddrs(self.canvas, self.primitive_funcs)
                    yield from addr.to_detaddrs(
                        self, self.canvas, self.primitive_funcs
                    )
                case SoupRef():
                    yield DetAddrWithSubst(subst, addr)
                case (i, j, f):
                    for painter in self.soups():
                        pi, pj, pf = painter
                        subst2 = subst.unify(addr, painter)
                        if subst2:
                            yield DetAddrWithSubst(subst2, painter)
                case _:
                    raise NotImplementedError(
                        f'Addr {addr} has unknown type {type(addr)}.'
                    )

    def soups(self) -> Soup:
        return Soup.union(self.lts, self.ws)

    def func_to_detfuncs(self, subst: Subst, var: Variable, func: Func) \
    -> Iterable[DetFunc]:  # TODO Create an appropriate type
        with indent_log(6, 'FUNC to DETFUNCS', func, subst):
            match func:
                case Variable():
                    if func in subst:
                        yield subst[func]
                    else:
                        yield from self.primitive_funcs
                case (i, j, f):
                    for ii, jj, ff in product(
                        self.addr_to_detaddrs(subst, I, i),
                        self.addr_to_detaddrs(subst, J, j),
                        self.func_to_detfuncs(subst, F, f)
                    ):
                        yield (ii.addr, jj.addr, ff)
                case _:
                    yield func

    def are_related_by(self, i: Index, j: Index, f: Func) -> bool:
        if not (self.canvas.has_letter(i) and self.canvas.has_letter(j)):
            return False
        # TODO take the Subst as an argument to are_related_by
        return self.apply_func(empty_subst, f, self.canvas[i]) == self.canvas[j]

    def apply_func(self, subst: Subst, f: Func, v: Value) \
    -> Union[Value, Painter]:
        with indent_log(6,
            f'APPLY FUNC {short(f)}({short(v)})  {short(subst)}'
        ):
            if isinstance(f, str) or isinstance(f, int) or is_painter(f):
                return f
            elif callable(f):
                return f(self, subst, v)
            else:
                raise NotImplementedError(f"apply_func: can't apply {f}")

    def state_str(self) -> str:
        sio = StringIO()
        print('canvas:', self.canvas.state_str(), file=sio)
        print(self.ws.state_str(), file=sio)
        return sio.getvalue()

    def __str__(self) -> str:
        return self.__class__.__name__

    __repr__ = state_str
