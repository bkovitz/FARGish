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

from Types import Addr, CanvasValue, F, Fizzle, FizzleValueNotFound, \
    Func, I, Index, Indices, J, MaybeIndex, Painter, SimpleFunc, SoupRef, \
    SpecialAddr, Value, Variable, WorkingSoup, \
    addr_str, func_str, is_painter, painter_str
import Types
#from BaseModel import Model
from Canvas import Canvas, Canvas1D
from Soup import Soup
from Subst import Subst, empty_subst, Plus
from Funcs import same, pred, succ
from Log import lo, trace, indent_log
from util import short, nf, Numeric, reseed


@dataclass(frozen=True)
class DetPainter:
    '''A determinate painter: it can paint one thing in one place; there is
    no matching or searching to be done in order to run it.'''
    subst: Subst
    source: DetAddr
    target: DetAddr
    func: DetFunc
    prob_weight: Numeric
    basis: Optional[Painter] = None  # what this DetPainter was made from

    @classmethod
    def make_from(cls, painter: Tuple[DetAddr, DetAddr, DetFunc]) -> DetPainter:
        '''An easy way to construct a DetPainter in a unit test. Not for use
        in the model proper.'''
        source, target, func = painter
        return cls(
            empty_subst.unify_ijf(source, target, func),
            source,
            target,
            func,
            1,
            None
        )

    def is_valid_for(self, canvas: Canvas) -> bool:
        match (self.source, self.target):
            case (int(), int()):
                return (
                    canvas.has_addr(self.source)
                    and
                    canvas.has_addr(self.target)
                )
            case _:
                return True

    def as_painter(self) -> Painter:
        return (self.source, self.target, self.func)

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'({short(self.subst)}; {addr_str(self.source)}, {addr_str(self.target)}, {func_str(self.func)}; {nf(self.prob_weight)})'

# A determinate Addr: no variables, no patterns, nothing to match or expand
DetAddr = Union[Index, Indices, Painter, SoupRef]

# A determinate Func: no variables
DetFunc = Union[Value, DetPainter, Callable] 

@dataclass(frozen=True)
class DetAddrWithSubst:
    subst: Subst
    addr: DetAddr

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.subst)}, {short(self.addr)})'

    __str__ = short
    __repr__ = short

@dataclass(frozen=True)
class RelatedPair(SpecialAddr):
    '''A kind of Addr: a RelatedPair matches any pair of canvas cells that
    are related by a function, like same, pred, or succ.'''
    i: Addr
    j: Addr
    f: Func

    def to_detaddrs(
        self,
        model: Model,
        canvas: Canvas,
        primitive_funcs: Iterable[Func]
    ) -> Iterable[DetAddrWithSubst]:
        match (self.i, self.j):
            case (Variable(), Variable()):
                for i in canvas.all_addrs():
                    for j in canvas.all_addrs():
                        if i >= j:
                            continue
                        for f in self.func_iter(self.f, primitive_funcs):
                            if model.are_related_by(i, j, f):
                                #lo('ARER', i, j ,f)
                                yield DetAddrWithSubst(
                                    Subst.make_from(
                                        (self.i, i), (self.j, j), (self.f, f)
                                    ),
                                    Indices(i, j)
                                )
            case _:
                raise NotImplementedError(
                    f"RelatedPair.to_detaddrs: can't search over ({self.i}, {self.j})"
                )

    def func_iter(self, f: Func, primitive_funcs: Iterable[Func]) \
    -> Iterable[Func]:
        match f:
            case Variable():
                yield from primitive_funcs
            case _:
                yield f
            # TODO Other cases? Could a Variable be in an expression?

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.i)}, {short(self.j)}, {short(self.f)})'

### Painter-building functions

@dataclass(frozen=True)
class MakeBetweenPainter:
    '''Makes a painter that paints a value between two others.'''
    i: Addr
    j: Addr
    f: Func

    def __call__(self, model: Model, subst: Subst, ignored: Value) -> Value:
        result_i = subst.as_index(self.i)
        if result_i is None:
            raise Fizzle  # TODO More-specific Fizzle
        value = model.canvas[result_i + 1]
        if value is None:
            raise Fizzle  # TODO More-specific Fizzle
        result_j = subst.as_index(self.j)
        if result_j is None:
            raise Fizzle  # TODO More-specific Fizzle
        result_f = subst[self.f]
        if result_f is None:
            raise Fizzle  # TODO More-specific Fizzle
        return (
            (I, Plus(I, result_j - result_i), result_f),
            WorkingSoup,
            (I, Plus(I, 1), value)
        )

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.i)}, {short(self.j)}, {short(self.f)})'

### The model

default_primitive_funcs: FrozenSet[Func] = frozenset([same, succ, pred])
default_initial_painters: List[Painter] = [
    (RelatedPair(I, J, F), WorkingSoup, (I, J, F)),
    #((I, J, SimpleFunc(F)), WorkingSoup, MakeRelativeIndirectPainter(I, J, F))
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

    @classmethod
    def canvas_from(cls, s: str) -> Model:
        return cls(canvas=Canvas1D.make_from(s))

    def set_canvas(self, s: str) -> None:
        self.canvas = Canvas1D.make_from(s)

    def contents_at(self, addr: Addr) -> Value:
        #TODO Look up a painter by addr?
        return self.canvas[addr]

    def absorb(self, s: str):
        self.set_canvas(s)
        #NEXT  
        #      Run from initial painters a while
        for t in range(2):
            self.do_timestep()
            print(self.state_str())
        #      Save the abstract ones to the lts
        #      Clear the ws

    def do_timestep(self) -> None:
        self.t += 1
        # decay
        lo(f't={self.t}')
        self.run_detpainter(self.choose_detpainter(self.soups()))

    def choose_detpainter(self, soup: Soup) -> DetPainter:
        det_painters = list(chain.from_iterable(
            self.painter_to_detpainters(p)  #, soup.clarity(p))
                for p in soup
        ))
        weights = [
            self.detpainter_to_probability_weight(dp)
                for dp in det_painters
        ]
        # logging
        if det_painters:
            for ii in range(len(det_painters)):
                lo(det_painters[ii], nf(weights[ii]))
            lo()
        else:
            lo('No det_painters!')

        ii = choices(range(len(det_painters)), weights)[0]
        dp = det_painters[ii]
        lo('dp =', dp, '  ', nf(weights[ii]))

        return dp

    def detpainter_to_probability_weight(self, dp: DetPainter) -> Numeric:
        return (
            self.source_weight(dp.source)
            *
            self.target_weight(dp.target)
            *
            dp.prob_weight
        )

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
        source, target, func = p

        det_sources = self.addr_to_detaddrs(empty_subst, I, source)
        det_sources = list(det_sources) #DEBUG
        lo('DETSRC', det_sources)

        source_target_pairs = (
            (ds, dt)
                for ds in det_sources
                    for dt in self.addr_to_detaddrs(ds.subst, J, target)
        )
        source_target_pairs = list(source_target_pairs) #DEBUG
        lo('STPAIRS', list(zip(*source_target_pairs)))
        #lo('DETTARG', source_target_pairs[1])

        triples = (
            (ds, dt, df)
                for (ds, dt) in source_target_pairs
                    for df in self.func_to_detfuncs(dt.subst, F, func)
        )
        triples = list(triples) #DEBUG
        lo('DETFUNCS', triples)  # we really want the 3rd "column"
        #lo('DETFUNCS', triples[2])

        return (
            DetPainter(
                dt.subst,
                ds.addr,
                dt.addr,
                df,
                1.0,   # TODO prob_weight,
                p  # basis, "author"
            ) for ds, dt, df in triples
        )

    # TODO Rename to addr_to_detaddrs_with_subst
    def addr_to_detaddrs(self, subst: Subst, var: Variable, addr: Addr) \
    -> Iterable[DetAddrWithSubst]:
        with indent_log(5, 'A2DS', addr, addr in subst, subst[addr]):
            match addr:
                case int():
                    yield DetAddrWithSubst(subst.unify(var, addr), addr)
                case str():
                    yield from (
                        DetAddrWithSubst(subst.unify(var, index), index)
                            for index in self.canvas.all_matching(addr)
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
                case RelatedPair():
                    #yield from addr.to_detaddrs(self.canvas, self.primitive_funcs)
                    got = list(addr.to_detaddrs(
                        self, self.canvas, self.primitive_funcs)
                    )
                    lo(5, 'RPGOT', got)
                    yield from got
                case SoupRef():
                    yield DetAddrWithSubst(subst, addr)
                case (i, j, f):
                    for painter in self.soups():
                        pi, pj, pf = painter
                        lo(5, 'MATCHING PAINTERS', (i, j, f), (pi, pj, pf))
                        subst2 = subst.unify(i, pi).unify_if_undefined(I, pi)
                        if not isinstance(pj, SoupRef):  # HACK
                            subst2 = subst2.unify(j, pj).unify_if_undefined(J, pj)
                        subst2 = subst2.unify(f, pf)
                        subst2 = subst2.unify_if_undefined(F, pf)
                        if subst2:
                            if I not in subst2:
                                subst2 = subst2.unify(I, pi) #TODO Is this right?
                            yield DetAddrWithSubst(subst2, painter)
                case _:
                    raise NotImplementedError(
                        f'Addr {addr} has unknown type {type(addr)}.'
                    )

    def soups(self) -> Soup:
        return Soup.union(self.lts, self.ws)

    def func_to_detfuncs(self, subst: Subst, var: Variable, func: Func) \
    -> Iterable[DetFunc]:  # TODO Create an appropriate type
        #print('FTOD', short(subst), '\n', var, '\n', short(func))
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
    
m: Model

def run_test() -> None:
    global m
    m = Model.canvas_from('ajaqb')
    print(m.lts)
    m.absorb('ajaqb')


if __name__ == '__main__':
    if len(sys.argv) < 2:
        #seed = 4993487641984628738  #None
        seed = None
    else:
        seed = int(sys.argv[1])
    seed = reseed(seed)
    print(f'seed={seed}')
    print()

    run_test()
    #run_ajaqb('a    ', ['wxyaaaa'], 120)
    #run_abs()
