# Experiments.py  --  Configured runs of RMem

from __future__ import annotations
from dataclasses import dataclass, fields
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

from RMem import RMem, RMemAbs, RMemFuncs, Absorb, Regenerate, \
    CanvasToPainters, \
    Canvas, Canvas1D, CanvasAble, make_eqns, BaseValue, \
    Painter, Func, GSet, PSet, Value, SkewedClarityWeight
from Mixins import WithCountColumns, WithRandomSalt, WithSequentialSalt
from Harness import TestSpec, EquationMaker, PartialCueMaker
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
    n_per_eqn: int=3,
    n_eqns: int=20,
    niters: int=50,
    seed: int=None,
    operands=range(1, 11),
    operators=('+', '-', 'x', '/'),
    rm: Union[RMem, Type[RMem]]=RMemAbs,
    npartial: int=3,
) -> EqnCounter:
    reseed(seed)
    full_table = tuple(make_eqns(operands=operands, operators=operators))
    l = len(full_table[0])
    rmem: RMem
    if isclass(rm):
        #rmem = rm.make_from(full_table)
        rmem = rm().absorb_canvases(full_table)
    else:
        rmem = rm  # type: ignore[assignment]
        rmem.absorb_canvases(full_table)
    counter: EqnCounter = defaultdict(int)

    for eqn in sample_without_replacement(full_table, k=n_eqns):
        if show:
            print(eqn)
        for i in range(n_per_eqn):
            startc = partial_eqn(eqn, k=npartial)
            if show:
                lo('CUE', startc)
            got = rmem.regenerate(canvas=startc, niters=niters).as_tuple()
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

def just_1_1_2() -> RMem:
    rmem = RMemAbs()
    return rmem.absorb_canvases(
        make_eqns(operands=[1], operators=['+']),
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
    rmem = RMemAbs().absorb_canvases(
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
        got = rmem.regenerate(pad_tup(rel))
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
        rmem.regenerate(new_cue, niters=100)
        pr(rmem.lsteps)

"""
def xp2() -> None:
    '''Second-order painters'''  # TODO  Not working yet  22-Jan-2022
    rmem = RMem()
    # Make 1st-order painters for 1+1=2
    p1s = rmem.canvas_to_painters((1, '+', 1, '=', 2))
    pr(p1s)
    print()

    gs1 = rmem.painters_to_pset(p1s)

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
    #re_p1s = rmem.canvas_to_painters((1, '+', None, None, None))

    # Grow the first-order painters by running the 2nd-order painters.
"""

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
    rmem = RMemAbs()
    if start_columns is None:
        count_columns = (0,) * len(funcs)
    else:
        count_columns = start_columns
    history = [count_columns]
    for _ in range(max_iters):
        startc = count_columns + base_canvas
        if show:
            lo(startc)
        painters = rmem.canvas_to_painters(startc)
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
    rmem = RMemAbs()

    #eqn = (1, '+', 1, '=', 2)
    eqn = (2, '+', 1, '=', 3)
    count_columns = [0, 0]
    add1 = rmem.add_n(1)
    for i in range(10):
        startc: CanvasAble = tuple(count_columns) + eqn  # (count(same), count(+1))
        lo(startc)
        painters = rmem.canvas_to_painters(startc)
        count_columns[0] = sum(1 for (a, b, f) in painters if f == rmem.same)
        count_columns[1] = sum(1 for (a, b, f) in painters if f == add1)
    lo(tuple(count_columns) + eqn)

def xpg() -> Set[FrozenSet[Tuple]]:
    funcs = (
        RMemFuncs.same,
        RMemFuncs.add_n(1),
        RMemFuncs.mul_by(2),
        RMemFuncs.add_n(2),
        RMemFuncs.sub_n(1),  # putting this one in lengthens the cycles by a lot
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

RMemCC = RMemAbs.make_class((WithCountColumns,)) # type: ignore[assignment]
RMemSalt = RMemAbs.make_class((WithRandomSalt,)) # type: ignore[assignment]
RMemSeqSalt = RMemAbs.make_class((WithSequentialSalt,)) # type: ignore[assignment]

def xpgfid2() -> None:
    rmemcc = RMemCC.make_instance()
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
        rm=RMemCC.make_instance(funcs_to_count=(
            RMemFuncs.same,
            RMemFuncs.add_n(1),
            RMemFuncs.mul_by(2),
            RMemFuncs.add_n(2),
            RMemFuncs.sub_n(1),  # putting this one in lengthens the cycles by a lot
        )),
    )
    salt_kwargs = dict(
        rm=RMemSalt.make_instance(nsalt=5)
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

def big_run(**kwargs) -> None:
    start_time = perf_counter()
    eqns_params: List[Dict[str, Any]] = [
        #dict(operands=[1], operators=['+']),
        #dict(operands=[1, 2], operators=['+']),
        dict(operands=range(1, 4), operators=['+']),
        #dict(operands=range(1, 6), operators=['+']),
        dict(operands=range(1, 11), operators=['+']),
        #dict(operands=range(1, 11), operators=['+', 'x']),
        #dict(operands=range(1, 4), operators=['+', '-', 'x', '/']),
        #dict()
    ]
    for eqn_ps in eqns_params:
        #for niters in [20, 60, 100, 200, 500]:
        for niters in [150]:
        #for niters in [1000]:
            cls: Type[RMem]
            clss: Sequence[Type[RMem]] = [RMemAbs, RMemCC, RMemSalt, RMemSeqSalt]
            for cls in clss:
                #print() #DEBUG
                #print(cls.__name__) #DEBUG
                #pts(fields(cls)) #DEBUG
                cl = cls.make_class((SkewedClarityWeight,))
                #print() #DEBUG
                #pts(fields(cl)) #DEBUG
                #print() #DEBUG
                kw = kwargs | dict(niters=niters) | eqn_ps
                tspec = TestSpec(
                    cls=cl,
                    kwargs=kw,
                    initial_canvases_cls=EquationMaker,
                    nsamples=50,
                    n_per_sample=10
                )
                result = tspec.run()
                print(result.nstr())
                #print(kwargs)  #DEBUG
                #print(result.rmem.termination_threshold) # type: ignore  #DEBUG

    print(f'total time: {perf_counter() - start_time:1.3f} sec')

                    # How to make class-specific arg sets?
                    # call cartesian_product?

def little_run() -> None:
    global rmem
    #rmem = RMemSalt(nsalt=10, niters=100)

    rmem = RMemSeqSalt.make_instance(niters=1000)
    eqnmaker = EquationMaker()
    lo('HERE1')
    rmem.absorb_canvases(eqnmaker())
    lo('HERE2')

    rmem.run1([2, '+', None, None, 5])

    

if __name__ == '__main__':
    #counter = eqn_test(operands=range(1, 3), operators=['+'], show=True, prep=correction_redundancy(2, 2), n_per_eqn=10, n_eqns=5, niters=50)

    #lcsets = xpg()
    #print('number of limit cycles found:', len(lcsets))

    #big_run()
    #big_run(npartial=None)
    big_run(termination_threshold=3)
    #big_run(termination_threshold=5, npartial=None)

    #little_run()

    '''
    rmem = RMem()
    startc1 = (1, '+', 1, '=', 2)
    p1s1 = rmem.canvas_to_painters(startc1)
    p2s1 = rmem.make_gset(rmem.make_next_order_painters(p1s1))

    startc2 = (1, '+', 2, '=', 3)
    p1s2 = rmem.canvas_to_painters(startc2)
    p2s2 = rmem.make_gset(rmem.make_next_order_painters(p1s2))
    pr(p2s2)
    print()
    print()

    a1set = rmem.add_two_gsets(rmem.make_gset(p1s1), rmem.make_gset(p1s2))
    a2set = rmem.add_two_gsets(p2s1, p2s2)

    pr(a1set)
    print()
    print()
    pr(a2set)
    '''
