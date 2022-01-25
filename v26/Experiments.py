# Experiments.py  --  Configured runs of RMem

from __future__ import annotations
from dataclasses import dataclass
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from random import random
import operator
from functools import reduce
from collections import defaultdict
from random import randrange
from inspect import isclass
from time import perf_counter

from RMem import RMem, Canvas, Canvas1D, CanvasAble, CanvasPrep, make_eqns, \
    no_prep, ndups, BaseValue, correction_redundancy, \
    Painter, Func, GSet, PSet, Value
from Log import lo, trace
from util import pts, pr, ps, pss, psa, pl, as_tuple, reseed, \
    sample_without_replacement


rmem: RMem

def exp1():
    global rmem
    rmem = RMem.run(
        operands=range(1, 4),
        startc=(None, '+', 1, None, 3),
        ndups=2,
        niters=1000
    )
    pr(rmem.lsteps)
    
# TODO rm for partial_tup
def partial_eqn(eqn: Tuple[BaseValue, ...], k: int=3) -> Tuple[BaseValue, ...]:
    r = range(len(eqn))
    addrs = set(sample_without_replacement(r, k=k))
    return tuple(
        eqn[a] if a in addrs else None
            for a in r
    )

EqnCounter = Dict[Tuple[BaseValue, ...], int]

def eqn_test(  # TODO rename eqns_test
    show: bool=False,
    prep: CanvasPrep=no_prep,
    n_per_eqn: int=3,
    n_eqns: int=20,
    niters: int=50,
    seed: int=None,
    operands=range(1, 11),
    operators=('+', '-', 'x', '/'),
    rm: Union[RMem, Type[RMem]]=RMem,
    npartial: int=3,
) -> EqnCounter:
    reseed(seed)
    full_table = tuple(make_eqns(operands=operands, operators=operators))
    l = len(full_table[0])
    rmem: RMem
    if isclass(rm):
        rmem = rm.make_from(full_table, prep=prep)
    else:
        rmem = rm  # type: ignore[assignment]
        rmem.absorb_canvases(full_table, prep=prep)
    counter: EqnCounter = defaultdict(int)

    for eqn in sample_without_replacement(full_table, k=n_eqns):
        if show:
            print(eqn)
        for i in range(n_per_eqn):
            startc = partial_eqn(eqn, k=npartial)
            if show:
                lo('CUE', startc)
            got = rmem.run_gset(canvas=startc, niters=niters).as_tuple()
            if show:
                lo('GOT', got)
            if got[-len(eqn):] == eqn:
                counter[eqn] += 1
        if show:
            print()
        else:
            print('.', end='')
    if not show:
        print()
    return counter

def xp_single():
    global rmem
    rmem = RMem.run(
        operands=range(1, 2),
        operators=('+'),
        startc=(None, '+', None, None, None),
        ndups=1,
        niters=1000
    )
    pr(rmem.lsteps)

def just_1_1_2(prep: CanvasPrep=no_prep) -> RMem:
    return RMem.make_from(
        make_eqns(operands=[1], operators=['+']),
        prep=prep
    )

def xp_single2():
    global rmem
    rmem = RMem.run(
        operands=range(1, 2),
        operators=('+'),
        startc=(None, '+', 2, None, None),
        ndups=1,
        niters=1000
    )
    pr(rmem.lsteps)

def pad_tup(tup: Tuple) -> Tuple:
    return tuple(None for _ in range(2 * len(tup))) + tup

def xp1() -> None:
    global rmem
    rmem = RMem.make_from(
        pad_tup(e) for e in make_eqns([1], ['+'])
    )
    new_eqn = (2, '+', 1, '=', 3)
    cues = [   # Parts of 2 + 1 = 3
        (2, '+', None, None, None),
        (None, None, 1, '=', 3),
        (2, '+', None, None, 3)
    ]
    relateds: Set[Tuple] = set()
    '''
    for cue in cues:
        print(rmem.run_gset(cue))
    '''
    while len(relateds) < 2:
        rel = tuple(x if random() < 0.3 else None for x in new_eqn)
        if all(x is None for x in rel):
            continue
        print(rel)
        got = rmem.run_gset(pad_tup(rel))
        print(got)
        #relateds.add(as_tuple(got.contents)[-5:])
        relateds.add(as_tuple(got)[-5:])
    print()
    pr(relateds)

    new_canvas = reduce(operator.add, relateds) + new_eqn
    rmem.absorb_canvases([new_canvas])

    new_cue = (None,) * 10 + (None, '+', 1, None, 3)
    for _ in range(1):
        print()
        rmem.run_gset(new_cue, niters=100)
        pr(rmem.lsteps)

def xp2() -> None:
    '''Second-order painters'''  # TODO  Not working yet  22-Jan-2022
    rmem = RMem()
    # Make 1st-order painters for 1+1=2
    p1s = rmem.make_generators((1, '+', 1, '=', 2))
    pr(p1s)
    print()

    gs1 = rmem.make_gset(p1s)

    # Make 2nd-order painters
    # NEXT Need to cycle through the p1s and create a gset.
    p2s: List[Painter] = list(rmem.make_next_order_painters(p1s))
    #pr(p2s)
    for p2 in p2s:
        (xa, xb), (ya, yb), f = p2  # type: ignore[misc]
        lo(gs1[(xa, xb)], gs1[(ya, yb)])  # type: ignore[has-type]
        lo(p2)
        print()

    #rmem.absorb_canvases(p1s)
    #pr(rmem.gset)

    # Make first-order painters for partial canvas: (1, '+', None, None, None)
    #re_p1s = rmem.make_generators((1, '+', None, None, None))

    # Grow the first-order painters by running the 2nd-order painters.

def count_fs(painters: Iterable[Painter], func: Func) -> int:
    return sum(
        1
            for (a, b, f) in painters
                if f == func
    )

def find_limit_cycle(
    base_canvas: tuple,
    funcs: Collection,  # Collection[Func]
    start_columns: Optional[Tuple[int, ...]]=None,
    max_iters: int=100,
    show: bool=False
) -> Sequence[Tuple[int, ...]]:
    global rmem
    rmem = RMem()
    if start_columns is None:
        count_columns = (0,) * len(funcs)
    else:
        count_columns = start_columns
    history = [count_columns]
    for _ in range(max_iters):
        startc = count_columns + base_canvas
        if show:
            lo(startc)
        painters = rmem.make_generators(startc)
        count_columns = tuple(
            count_fs(painters, f) for f in funcs
        )
        try:
            index = history.index(count_columns)
        except ValueError:
            history.append(count_columns)
            continue
        if show:
            pts(history[index:])
        lo(f'    len={len(history) - index}   starts at: {history[index]}   index={index}')
#        lo(f'    ends at: {history[-1]}')
#        if len(history) - index != 48:
#            raise Exception
        return history[index:]
    return []

def xpg0() -> None:
    '''Simple experiment with global parameters: counts of certain types of
    Funcs appended to the Canvas.'''
    global rmem
    rmem = RMem()

    #eqn = (1, '+', 1, '=', 2)
    eqn = (2, '+', 1, '=', 3)
    count_columns = [0, 0]
    add1 = rmem.add_n(1)
    for i in range(10):
        startc = tuple(count_columns) + eqn  # (count(same), count(+1))
        lo(startc)
        painters = rmem.make_generators(startc)
        count_columns[0] = sum(1 for (a, b, f) in painters if f == rmem.same)
        count_columns[1] = sum(1 for (a, b, f) in painters if f == add1)
    lo(tuple(count_columns) + eqn)

def xpg() -> Set[FrozenSet[Tuple]]:
    funcs = (
        RMem.same,
        RMem.add_n(1),
        RMem.mul_by(2),
        RMem.add_n(2),
        RMem.sub_n(1),  # putting this one in lengthens the cycles by a lot
    )
    lcsets: Set[FrozenSet[Tuple]] = set()  # limit cycles, each unordered
    #for eqn in make_eqns(operands=range(1, 4), operators=['+', '-']):
    #for eqn in make_eqns():
    #for eqn in [(9, '+', 9, '=', 18)]:
    for eqn in [(2, '+', 1, '=', 3)]:
        for startn in range(1000):
            #start_columns = (startn,) * len(funcs)
            start_columns = tuple(randrange(10) for _ in range(len(funcs)))
            lo(eqn)
            lc = find_limit_cycle(eqn, funcs, start_columns, show=False)
            lcsets.add(frozenset(lc))
            print()
    return lcsets

@dataclass
class WithCountColumns(RMem):
    funcs_to_count: Tuple[Func, ...] = (
        RMem.same,
        RMem.add_n(1)
    )

    def raw_absorb_canvas(self, c: Canvas) -> None:
        for pset in self.limit_cycle_of_psets(c):
            self.raw_absorb_gset(pset)

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
            pset = self.make_gset(self.make_generators(startc))
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

@dataclass
class WithRandomSalt(RMem):
    '''Prepends cells containing random numbers (the 'salt') to every canvas
    absorbed.'''
    nsalt: int = 30  # number of cells to prepend
    saltrange: Tuple[int, int] = (0, 11) # args to randrange for each salt cell
    # NEXT1 number of copies of each image to absorb
    # NEXT2 specify number of regeneration iterations somewhere

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

def xpgfid() -> None:
    '''Fidelity test: Does adding global count-columns enable the regeneration
    process to restore the original equation more often?'''
    # New absorb_canvas():
    #   For each pset in the limit cycle:
    #      raw_absorb it
    # Measure fidelity on grade-school table without count-columns.
    # Measure fidelity on grade-school table with count-columns.
    cls = type('RM', (WithCountColumns, RMem), {})
    rmem = cls()
    #eqn = (2, '+', 1, '=', 3)
    eqns = list(make_eqns(operands=range(1, 4), operators=['+']))
    for eqn in eqns:
        rmem.absorb_canvas(eqn)
    for eqn in eqns:
        startc = (None, None) + partial_eqn(eqn, k=4)
        got = rmem.run_gset(startc, niters=1000)
        lo(eqn)
        lo(startc)
        lo(got)
        lo()
    pr(rmem.lsteps)
    print()
    lo(startc)
    lo(eqn)

RMemCC = type('RMemCC', (WithCountColumns, RMem), {})
RMemSalt = type('RMemSalt', (WithRandomSalt, RMem), {})

def xpgfid2() -> None:
    rmemcc = RMemCC()
    common_kwargs = dict(
        #operands=[1, 2],
        operands=[1, 2, 3, 4, 5, 6],
        #operators=['+'],
        operators=['+', '-', 'x', '/'],
        npartial=4,  #3
        n_per_eqn=3, #100,
        niters=100,   #100
        n_eqns=5,  #20
        show=True
    )
    basic_kwargs: Dict = dict(
    )
    cc_kwargs = dict(
        rm=RMemCC(funcs_to_count=(
            RMem.same,
            RMem.add_n(1),
            RMem.mul_by(2),
            RMem.add_n(2),
            RMem.sub_n(1),  # putting this one in lengthens the cycles by a lot
        )),
    )
    salt_kwargs = dict(
        rm=RMemSalt(nsalt=5)
    )
    for kwargs in [basic_kwargs, cc_kwargs, salt_kwargs]:
    #for kwargs in [salt_kwargs]:
        print()
        pr(kwargs)
        start_time = perf_counter()
        result = eqn_test(
            #show=True,
            **(common_kwargs | kwargs)  # type: ignore[arg-type]
        )
        test_duration = perf_counter() - start_time
        pr(result)
        lo(f'{sum(result.values())}      {test_duration:8.3f} sec')

if __name__ == '__main__':
    #counter = eqn_test(operands=range(1, 3), operators=['+'], show=True, prep=correction_redundancy(2, 2), n_per_eqn=10, n_eqns=5, niters=50)

    #lcsets = xpg()
    #print('number of limit cycles found:', len(lcsets))

    xpgfid2()
