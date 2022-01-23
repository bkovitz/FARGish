# Experiments.py  --  Configured runs of RMem

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from random import random
import operator
from functools import reduce
from collections import defaultdict
from random import randrange

from RMem import RMem, Canvas, CanvasPrep, make_eqns, no_prep, ndups, \
    BaseValue, correction_redundancy, Painter, Func
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
    operators=('+', '-', 'x', '/')
) -> EqnCounter:
    reseed(seed)
    full_table = tuple(make_eqns(operands=operands, operators=operators))
    l = len(full_table[0])
    rmem = RMem.make_from(full_table, prep=prep)
    counter: EqnCounter = defaultdict(int)

    for eqn in sample_without_replacement(full_table, k=n_eqns):
        if show:
            print(eqn)
        for i in range(n_per_eqn):
            startc = partial_eqn(eqn)
            if show:
                lo('CUE', startc)
            got = rmem.run_gset(canvas=startc, niters=niters).as_tuple()
            if show:
                lo('GOT', got)
            if got == eqn:
                counter[eqn] += 1
        if show:
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


if __name__ == '__main__':
    #counter = eqn_test(operands=range(1, 3), operators=['+'], show=True, prep=correction_redundancy(2, 2), n_per_eqn=10, n_eqns=5, niters=50)
    lcsets = xpg()
    print('number of limit cycles found:', len(lcsets))
