# Painters.py -- Special types of Painter

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from io import StringIO
from itertools import chain, product

from Canvas import Canvas
from Addrs import Addr, DetAddr, DetAddrWithSubst, F, I, J, make_addr, \
    make_detaddr, \
    ToAddr, ToDetAddr, Variable
from Funcs import CallableFunc, DetFunc, Func, make_detfunc, ToDetFunc, Value
from Subst import Subst, empty_subst
import Model as MM
from Log import indent_log, lo, set_log_level
from util import short, nf, Numeric


@dataclass(frozen=True)
class Painter(Addr, CallableFunc):
    source: Addr
    target: Addr
    func: Func

    @classmethod
    def make_from(cls, i: ToAddr, j: ToAddr, f: Func) -> Painter:
        '''An easy way to construct a Painter in a unit test. Not for use
        in the model proper.'''
        return Painter(make_addr(i), make_addr(j), f)

    def to_detaddrs(self, model: MM.Model, subst: Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        for painter in model.soups():
            subst2 = subst.unify(self, painter)
            if subst2:
                yield DetAddrWithSubst(subst2, painter)

    def to_detpainters(self, model: MM.Model) -> Iterable[DetPainter]:
        #source, target, func = p

        det_sources = list(self.source.to_detaddrs(model, empty_subst, I))
        with indent_log(8, 'DET SOURCES'):
            for det_source in det_sources:
                lo(8, det_source)

        source_target_pairs = (
            (ds, dt)
                for ds in det_sources
                    #for dt in self.addr_to_detaddrs(ds.subst, J, self.target)
                    for dt in self.target.to_detaddrs(model, ds.subst, J)
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
                    #for df in self.func_to_detfuncs(dt.subst, F, self.func)
                    #for df in self.func.to_detfuncs(model, dt.subst, F)
                    for df in model.func_to_detfuncs(dt.subst, F, self)
        )
        triples = list(triples) #DEBUG
        #lo(7, 'DETFUNCS', triples)  # we really want the 3rd "column"
        #lo(7, 'DETFUNCS', triples[2])
        with indent_log(7, 'DET TRIPLES'):
            for triple in triples:
                lo(7, triple)

        for ds, dt, df in triples:
            dp = DetPainter(
                dt.subst,
                ds.addr,
                dt.addr,
                df,
                model.suppression((ds.addr, dt.addr, df)),
                    # TODO figure painter clarity into prob_weight?
                self  # basis, "author"
            )
            lo(5, dp)
            yield dp

    def to_detfuncs(self, model: MM.Model, subst: Subst, var: Variable) \
    -> Iterable[DetFunc]:
        for i, j, f in product(
            self.source.to_detaddrs(model, subst, I),
            self.target.to_detaddrs(model, subst, J),
            model.func_to_detfuncs(subst, F, self.func),
        ):
            yield Painter(i.addr, j.addr, f)

    def apply(self, model: MM.Model, subst: Subst, value: Value) \
    -> Value:
        return self

@dataclass(frozen=True)
class CPainter(Painter):
    '''A canvas-painter.'''
    pass

@dataclass(frozen=True)
class PPainter(Painter):
    '''A painter-painter.'''
    pass


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
    def make_from(cls, triple: Tuple[ToDetAddr, ToDetAddr, ToDetFunc]) \
    -> DetPainter:
        '''An easy way to construct a DetPainter in a unit test. Not for use
        in the model proper.'''
        source = make_detaddr(triple[0])
        target = make_detaddr(triple[1])
        func = make_detfunc(triple[2])
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

    def as_painter(self) -> DetPainter0:
        return (self.source, self.target, self.func)

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.as_painter()):80s}; {short(self.subst):30s}; {nf(self.prob_weight)})'

    def __str__(self) -> str:
#        sio = StringIO()
#        print(short(self.as_painter()), file=sio)
#        print(short(self.subst), file=sio)
#        print(nf(self.prob_weight), file=sio)
#        print(f'basis={short(self.basis)}', file=sio)
#        return sio.getvalue()
        pstr = short(self.as_painter())
        sstr = short(self.subst)
        bstr = f'basis={short(self.basis)}'
        return f'{pstr}; {sstr}; {nf(self.prob_weight)}; {bstr}'

# A determinate painter. Unlike DetPainter, a DetPainter0 includes only the
# minimal painter info, not additional information like a Subst.
DetPainter0 = Tuple[DetAddr, DetAddr, DetFunc]
