# Mixins.py -- Classes to mix into RMem to implement variations on
#              regenerative memory

from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from random import randrange, choices
from itertools import chain, islice

from RMem import RMem, Absorb, Regenerate, Func, RMemFuncs, PSet, \
    Canvas, CanvasAble, CanvasToPainters, ClarityWeight, Addr, Value, \
    Painter, AbsPainter
from Log import lo, trace
from util import pts, short, as_tuple, Numeric


@dataclass  # type: ignore[misc]
class WithCountColumns(CanvasToPainters, Absorb, Regenerate):
    mixin_name: ClassVar[str] = 'CoCo'

    funcs_to_count: Tuple[Func, ...] = (
        RMemFuncs.same,
        RMemFuncs.add_n(1)
    )

    """
    def raw_absorb_canvas(self, c: Canvas) -> None:
        for pset in self.limit_cycle_of_psets(c):
            self.raw_absorb_gset(pset)
    """
    def canvas_to_psets(self, c: CanvasAble) -> Iterable[PSet]:
        return self.limit_cycle_of_psets(c)

    """
    def prep_absorb(self, c: CanvasAble) -> CanvasAble:
        prefix = (None,) * len(self.funcs_to_count)
        return super().prep_absorb(prefix + as_tuple(c))
    """

    def prep_regen(self, c: CanvasAble) -> CanvasAble:
        prefix = (None,) * len(self.funcs_to_count)
        return super().prep_regen(prefix + as_tuple(c))

    def limit_cycle_of_psets(
        self,
        canvas: CanvasAble,
        funcs: Optional[Collection[Func]]=None,
        max_iters: int=100,
        show: bool=False
    ) -> Sequence[PSet]:
        '''The canvases that this makes psets for will have extra columns
        prepended for the painter counts.''' # TODO Clean that up so calling
        # code isn't bug-prone.
        base_tuple = as_tuple(canvas)
        if funcs is None:
            funcs = self.funcs_to_count
        count_columns = (0,) * len(funcs)
        cc_history = [count_columns]
        pset_history: List[PSet] = []
        for _ in range(max_iters):  # loop until a limit cycle completes
            startc = count_columns + base_tuple
            if show:
                lo(startc)
            pset = self.painters_to_pset(self.canvas_to_painters(startc))
            pset_history.append(pset)
            count_columns = tuple(
                sum(1 for f in pset.values() if f == func)
                    for func in funcs
            )
            try:
                index = cc_history.index(count_columns)
            except ValueError:
                cc_history.append(count_columns)
                continue
            if show:
                pts(cc_history[index:])
                lo(f'    len={len(cc_history) - index}   starts at: {cc_history[index]}   index={index}')
            cycle_len = len(cc_history) - index
            return pset_history[-cycle_len:]

        return []

    """
    def funcs_to_count(self) -> Collection[Func]:
        # TODO Optimize? It shouldn't be necessary to recreate these objects
        # on every call to .limit_cycle_of_psets().
        return [
            self.same,
            self.add_n(1)
        ]
    """

    """
    # TODO Move this to a separate mix-in.
    @classmethod
    def int_func_from_to(cls, x1: int, x2: int) -> Func:
        '''Returns only 'same' or addition/subtraction Funcs.'''
        if x1 == x2:
            return cls.same
        elif x1 > x2:
            return cls.sub_n(x1 - x2)
        else:
            return cls.add_n(x2 - x1)
    """

@dataclass  # type: ignore[misc]
class WithRandomSalt(Absorb, Regenerate):
    '''Prepends cells containing random numbers (the 'salt') to every canvas
    absorbed.'''
    mixin_name: ClassVar[str] = 'RandomSalt'

    nsalt: int = 10  # number of cells to prepend
    saltrange: Tuple[int, int] = (0, 11) # args to randrange for each salt cell
    # TODO number of copies of each image to absorb (for fair comparison
    # with WithCountColumns).

    def prep_absorb(self, c: CanvasAble) -> CanvasAble:
        prefix = tuple(randrange(*self.saltrange) for _ in range(self.nsalt))
        return super().prep_absorb(prefix + as_tuple(c))

    def prep_regen(self, c: CanvasAble) -> CanvasAble:
        prefix = (None,) * self.nsalt
        return super().prep_regen(prefix + as_tuple(c))

    # TODO Factor this out
    """
    def termination_condition(self, canvas: Canvas) -> bool:
        threshold = 0.8 * canvas.MAX_CLARITY  # 0.5
        #return all(cl >= threshold for cl in canvas.clarities)
        return all(cl >= threshold for cl in list(canvas.all_clarities())[-5:])
    """

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({self.nsalt})'

@dataclass  # type: ignore[misc]
class WithSequentialSalt(Absorb, Regenerate):
    '''Prepends calls containing numbers in sequence: 1, 2, 3, ... to every
    canvas absorbed.'''
    mixin_name: ClassVar[str] = 'SeqSalt'

    nseqsalt: int = 5

    def prep_absorb(self, c: CanvasAble) -> CanvasAble:
        prefix = tuple(range(1, self.nseqsalt + 1))
        return super().prep_absorb(prefix + as_tuple(c))

    def prep_regen(self, c: CanvasAble) -> CanvasAble:
        prefix = (None,) * self.nseqsalt
        return super().prep_regen(prefix + as_tuple(c))

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({self.nseqsalt})'

@dataclass  # type: ignore[misc]
class WithVoting(RMemFuncs, Regenerate, ClarityWeight):
    '''When regenerating, all runnable painters "vote" on the value to paint.
    This is closer to the Hopfield net, where the value painted into each
    target cell is determined by simultaneous contributions from all edges
    and hence all other cells.'''

    def regenerate(
        self,
        canvas: CanvasAble,
        pset: Optional[PSet]=None,
        niters: Optional[int]=None,
        vv: int=0  # verbosity
    ) -> Canvas:
        pset: PSet = self.pset if pset is None else pset
        niters: int = self.niters if niters is None else niters
        canvas: Canvas = self.as_canvas(self.prep_regen(canvas))
        for i in range(self.niters):
            addr = self.choose_target_addr(canvas)
            values, weights = zip(*chain.from_iterable(
                self.weighted_values(canvas, p)
                    for p in self.all_runnable_painters(canvas, addr, pset)
            ))
            #print()
            #pr(zip(values, weights))
            #value = choices(values, weights)[0]
            value = self.choose_value(values, weights)
            #lo('chose:', value)
            #print()
            canvas[addr] = value
            if self.termination_condition(canvas):
                break
        return canvas

    @classmethod
    def choose_value(
        self,
        values: Iterable[Value],   # these must have the same number of elems
        weights: Iterable[Numeric]
    ) -> Value:
        weights: List[Numeric] = list(weights)
        i = max(range(len(weights)), key=weights.__getitem__)
        return next(islice(values, i, None))

    def choose_target_addr(self, canvas: Canvas) -> Addr:
        addrs = list(canvas.all_addrs())
        weights = [
            self.to_clarity_weight(canvas, cl) for cl in canvas.all_clarities()
        ]
        return choices(addrs, weights)[0]

    def all_runnable_painters(
        self,
        canvas: Canvas,
        addr: Addr,
        pset: Optional[PSet]=None
    ) -> Iterable[AbsPainter]:
        '''Returns a generator yielding all painters that paint to addr, from
        an addr with a clarity > 0.'''
        if not isinstance(addr, int):
            raise NotImplementedError
        pset: PSet = self.pset if pset is None else pset
        for p in self.abs_painters_only(pset):
            a, b, f = p
            if b == addr and canvas.clarity(a) > 0:
                yield p

    @classmethod
    def weighted_values(cls, canvas: Canvas, p: AbsPainter) \
    -> Iterable[Tuple[Value, Numeric]]:
        a, b, func = p
        x = canvas[a]
        if x is None:
            return
        wcl = cls.from_clarity_weight(canvas, canvas.clarity(a))
        if abs(wcl) < cls.epsilon:
            return
        for f, w in cls.weighted_funcs(p):
            yweight = w * wcl * cls.natural_func_weight(f)
            if abs(yweight) < cls.epsilon:
                continue
            y = cls.apply_func(f, x)
            if y is not None:
                yield (y, w * wcl)

    @classmethod
    def weighted_funcs(cls, p: Painter) -> Iterable[Tuple[Func, Numeric]]:
        '''The weights of all the funcs for one painter should be 1.0.'''
        _, _, f = p
        if hasattr(f, 'weighted_funcs'):
            yield from f.weighted_funcs()  # type: ignore[union-attr, attr-defined]
        else:
            yield (f, 1.0)

@dataclass  # type: ignore[misc]
class WithRelsPaintAbsolutes(RMem):

    """
    def make_abs_painter(self, c: Canvas, p: Painter) -> AbsPainter:
        if self.is_abs_painter(p):
            return p
        # NEXT Is this already implemented by .as_abs_painters()?
    """
    pass
