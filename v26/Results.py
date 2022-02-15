from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING

from RMem import make_eqns, RMemAble, RMemAbs, WithNDups
from Harness import TestSpec2, EquationMaker, PartialCueMaker
from util import dupdate


eqns_123  = list(make_eqns(operands=[1, 2, 3], operators=['+']))
eqns_12345 = list(make_eqns(operands=[1, 2, 3, 4, 5], operators=['+']))
eqns_2ops3  = list(make_eqns(operands=[1, 2, 3], operators=['+', 'x']))
eqns_all = list(make_eqns())

basic = TestSpec2(
    rmem=RMemAbs(),
    nsamples=400,
)

def run(rmem: RMemAble) -> None:
    for eqns in [eqns_123, eqns_12345, eqns_2ops3, eqns_all]:
        for niters in [20, 40, 60, 80, 100, 200, 1000]:
            tspec = TestSpec2(
                rmem=rmem,
                initial_canvases=eqns,
                kwargs=dict(niters=niters),
                nsamples=400,
            )
            # TODO Print short form of tspec
            result = tspec.run(vv=1)
            print(result.nstr())
            print()

if __name__ == '__main__':
    run(RMemAbs)
    #run((WithNDups, RMemAbs))
