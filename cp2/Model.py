# Model.py -- The canvas-and-painters model

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod
from itertools import chain, product

from Types import Addr, CanvasValue, F, Fizzle, FizzleValueNotFound, \
    Func, I, Index, Indices, J, MaybeIndex, Painter, SoupRef, Value, Variable, \
    addr_str, func_str, is_painter, painter_str
import Types
from Canvas import Canvas, Canvas1D
from Soup import Soup
from Subst import Subst, empty_subst, Plus
#from Funcs import same, pred, succ
from Log import lo, trace
from util import short, nf, Numeric


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
        return f'({short(self.subst)}, {addr_str(self.source)}, {addr_str(self.target)}, {func_str(self.func)}; {nf(self.prob_weight)})'

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

@dataclass(frozen=True)
class RelatedPair:
    '''A kind of Addr: a RelatedPair matches any pair of canvas cells that
    are related by a function, like same, pred, or succ.'''
    i: Addr
    j: Addr
    f: Func

    def to_detaddrs(self, canvas: Canvas, primitive_funcs: Iterable[Func]) \
    -> Iterable[DetAddrWithSubst]:
        match (self.i, self.j):
            case (Variable(), Variable()):
                for i in canvas.all_addrs():
                    for j in canvas.all_addrs():
                        if i >= j:
                            continue
                        for f in self.func_iter(self.f, primitive_funcs):
                            if canvas.are_related_by(i, j, f):
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

### The basic relational functions

def same(subst: Subst, v: Value) -> Value:
    return v

def succ(subst: Subst, v: Value) -> Value:
    # TODO Deal with 'z'
    if isinstance(v, str):
        return chr(ord(v) + 1)
    elif isinstance(v, int):
        return v + 1
    raise Fizzle("succ: Can't take successor of {v}")

def pred(subst: Subst, v: Value) -> Value:
    # TODO Deal with 'a'
    if isinstance(v, str):
        return chr(ord(v) - 1)
    elif isinstance(v, int):
        return v - 1
    raise Fizzle("pred: Can't take predecessor of {v}")

### The constant function

@dataclass(frozen=True)
class const:
    v: Value

    def __call__(self, subst: Subst, ignored: Value) -> Value:
        return self.v

    def short(self) -> str:
        cl = self.__class__.__name__
        if is_painter(self.v):
            return f'{cl}({painter_str(self.v)})'
        else:
            return f'{cl}({short(self.v)})'

### Painter-building functions

@dataclass(frozen=True)
class MakeBetweenPainter:
    i: Addr
    j: Addr
    f: Func

    def __call__(self, model: Model, subst: Subst, ignored: Value) -> Value:
        #return (self.i, self.j, model.contents_at(self.j))
        result_i = subst.as_index(self.i)
        if result_i is None:
            raise Fizzle  # TODO More-specific Fizzle
        result_j = subst.as_index(self.j)
        if result_j is None:
            raise Fizzle  # TODO More-specific Fizzle
        value = model.canvas[result_i + 1]
        if value is None:
            raise Fizzle  # TODO More-specific Fizzle
        result_f = (I, Plus(I, 1), value)
        return (result_i, result_j, result_f)

### The model

@dataclass
class Model:
    lts: Soup = field(default_factory=lambda: Soup())
    ws: Soup = field(default_factory=lambda: Soup())
    canvas: Canvas1D = field(
        default_factory=lambda: Canvas1D.make_from('     ')
    )

    primitive_funcs: FrozenSet = frozenset([same, succ, pred])

    @classmethod
    def canvas_from(cls, s: str) -> Model:
        return Model(canvas=Canvas1D.make_from(s))

    def set_canvas(self, s: str) -> None:
        self.canvas = Canvas1D.make_from(s)

    def contents_at(self, addr: Addr) -> Value:
        #TODO Look up a painter by addr?
        return self.canvas[addr]

    def run_detpainter(
        self,
        painter: Tuple[DetAddr, DetAddr, DetFunc],
        subst: Subst=empty_subst
    ) -> None:
        '''Runs a DetPainter, with caller-supplied Subst. This function does
        not get called during normal running of the model. It's a convenience
        for experimentation and unit testing.'''
        source, target, func = painter
        #lo('RUNDETP', source, target, func)
        match target:
            #TODO Painting to canvas
            case Types.WorkingSoup:
                match self.apply_func(subst, func, self.contents_at(source)):
                    case p if is_painter(p):
                        self.ws.add(p)
                    case x:
                        raise ValueError(f'run_detpainter: try to paint {x} (type {type(x)}) to the workspace.')
            #TODO Painting to long-term soup
            case _:
                raise NotImplementedError(f"run_detpainter: can't paint to target; painter={painter}")

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
        #lo('DETSRC', det_sources)

        source_target_pairs = (
            (ds, dt)
                for ds in det_sources
                    for dt in self.addr_to_detaddrs(ds.subst, J, target)
        )
        source_target_pairs = list(source_target_pairs) #DEBUG
        #lo('DETTARG', source_target_pairs[1])

        triples = (
            (ds, dt, df)
                for (ds, dt) in source_target_pairs
                    for df in self.func_to_detfuncs(dt.subst, F, func)
        )
        triples = list(triples) #DEBUG
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
                got = list(addr.to_detaddrs(self.canvas, self.primitive_funcs))
                #lo('RPGOT', got)
                yield from got
            case SoupRef():
                yield DetAddrWithSubst(subst, addr)
            case (i, j, f):
                for painter in self.soups():
                    pi, pj, pf = painter
                    subst2 = subst. \
                        unify(i, pi). \
                        unify_if_undefined(I, pi). \
                        unify(j, pj). \
                        unify_if_undefined(J, pj). \
                        unify(f, pf). \
                        unify_if_undefined(F, pf)
                    if subst2:
                        if I not in subst2:
                            subst2 = subst2.unify
                        yield DetAddrWithSubst(subst2, painter)
            case _:
                raise NotImplementedError(
                    f'Addr {addr} has unknown type {type(addr)}.'
                )

    def soups(self) -> Soup:
        return self.lts.union(self.ws)

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

    def apply_func(self, subst: Subst, f: Func, v: Value) \
    -> Union[Value, Painter]:
        if isinstance(f, str) or isinstance(f, int) or is_painter(f):
            return f
        elif callable(f):
            return f(self, subst, v)
        else:
            raise NotImplementedError(f"apply_func: can't apply {f}")

