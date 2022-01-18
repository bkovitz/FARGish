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

from RMem import RMem, Canvas, CanvasPrep, make_eqns, no_prep, ndups, \
    BaseValue, correction_redundancy
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

def xp() -> None:
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
        relateds.add(as_tuple(got.contents)[-5:])
    print()
    pr(relateds)

    new_canvas = reduce(operator.add, relateds) + new_eqn
    rmem.absorb_canvases([new_canvas])

    new_cue = (None,) * 10 + (None, '+', 1, None, 3)
    for _ in range(1):
        print()
        rmem.run_gset(new_cue, niters=100)
        pr(rmem.lsteps)

if __name__ == '__main__':
    counter = eqn_test(operands=range(1, 3), operators=['+'], show=True, prep=correction_redundancy(2, 2), n_per_eqn=10, n_eqns=5, niters=50)
