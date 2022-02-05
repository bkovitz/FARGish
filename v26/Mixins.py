# Mixins.py -- Classes to mix into RMem to implement variations on
#              regenerative memory

from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from random import randrange

from RMem import RMem, Absorb, Regenerate, Func, RMemFuncs, PSet, \
    CanvasAble, CanvasToPainters, RegenerateDataclassMixin
from Log import lo, trace
from util import pts, short, as_tuple


@dataclass  # type: ignore[misc]
class WithCountColumnsDataclassMixin(RegenerateDataclassMixin):
    pass

@dataclass  # type: ignore[misc]
class WithCountColumns(
    #WithCountColumnsDataclassMixin,
    CanvasToPainters, Absorb, Regenerate
):
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
class WithRandomSaltDataclassMixin(RMem):
    nsalt: int = 10  # number of cells to prepend
    saltrange: Tuple[int, int] = (0, 11) # args to randrange for each salt cell
    # NEXT1 number of copies of each image to absorb
    # NEXT2 specify number of regeneration iterations somewhere

class WithRandomSalt(WithRandomSaltDataclassMixin, Absorb, Regenerate):
    '''Prepends cells containing random numbers (the 'salt') to every canvas
    absorbed.'''
    mixin_name: ClassVar[str] = 'RandomSalt'

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
class WithSequentialSaltDataclassMixin(RMem):
    nseqsalt: int = 5

class WithSequentialSalt(WithSequentialSaltDataclassMixin, Absorb, Regenerate):
    '''Prepends calls containing numbers in sequence: 1, 2, 3, ... to every
    canvas absorbed.'''
    mixin_name: ClassVar[str] = 'SeqSalt'

    def prep_absorb(self, c: CanvasAble) -> CanvasAble:
        prefix = tuple(range(self.nseqsalt))
        return super().prep_absorb(prefix + as_tuple(c))

    def prep_regen(self, c: CanvasAble) -> CanvasAble:
        prefix = (None,) * self.nseqsalt
        return super().prep_regen(prefix + as_tuple(c))

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({self.nseqsalt})'


