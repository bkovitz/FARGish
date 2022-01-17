# Experiments.py  --  Configured runs of RMem

from __future__ import annotations
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence, Literal
from random import random
import operator
from functools import reduce

from RMem import RMem, make_eqns
from util import pts, pr, ps, pss, psa, pl, as_tuple


rmem: RMem = None

def exp1():
    global rmem
    rmem = RMem.run(
        operands=range(1, 4),
        startc=(None, '+', 1, None, 3),
        ndups=2,
        niters=1000
    )
    pr(rmem.lsteps)
    
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

def just_1_1_2() -> RMem:
    return RMem.make_from(
        make_eqns(operands=[1], operators=['+'], ndups=1)
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
    relateds: Set[Canvas] = set()
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
    for _ in range(10):
        print(rmem.run_gset(new_cue, niters=100))

