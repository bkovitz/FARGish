# atestPons.py -- Acceptance test: solve the Numbo pons asinorum
#
# "Given 4, 5, 6, can you make 15?"

import unittest
from pprint import pprint as pp
import inspect

from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable

from FARGModel import FARGModel, FARGException, SolvedPuzzle, CellRef, \
    Agent, Codelet, Fizzle
from Log import lenable, ldisable
from Propagator import LogAdjustedDeltas
from Graph import Graph, Before, After
from Slipnet import Slipnet
from Agents import Want, Consumer
from Codelets import RaiseException
from Canvas import StepCanvas, Step
from Equation import plus, minus, times
from util import pr, pts, short, trace

import matplotlib.pyplot as plt  # type: ignore[import]


eqn_graph = Graph.with_features(
    Consumer.make_table(
        range(1, 21), range(1, 21), [plus, minus, times]
    )
) #.add_edges(MutualInhibition((Feature, Operator, Equation, int), weight=-5.0))

def run(
    bricks: Sequence[int],
    target: int,
    seed: int=1,
    num_slipnet_iterations: Optional[int]=None
) -> None:
    global fm, ca, cr0, cr1, cr2, cr3, wa
    lenable(Agent, Codelet, Fizzle)
    fm = FARGModel(
        slipnet=Slipnet(eqn_graph),
        seed=seed,
        num_slipnet_iterations=num_slipnet_iterations
    )
    ca = fm.build(StepCanvas([Step(tuple(bricks))]))
    cr0 = CellRef(ca, 0)
    cr1 = CellRef(ca, 1)
    cr2 = CellRef(ca, 2)
    cr3 = CellRef(ca, 3)
    wa = fm.build(Want(
        startcell=cr0,
        target=target,
        on_success=RaiseException(SolvedPuzzle)
    ))

#    try:
    fm.do_timestep(num=20)
#    except Exception as exc:
#        print(exc)
    pr(fm, extra=True)


class TestPons(unittest.TestCase):

    def test_pons(self) -> None:
        run(bricks=[4, 5, 6], target=15)

if __name__ == '__main__':
    bricks = [4, 5, 6]
    target = 15
    #bricks = [40, 50, 60]
    #target = 150
    lenable(LogAdjustedDeltas)
    run(bricks=bricks, target=target, num_slipnet_iterations=20)
    als = fm.alogs
    print()
    pr(als)
    print()
    ls = list(als.logs.values())
    print('PULSED:')
    pr(ls[0].pulsed_nodes())
    pred = (Consumer, int)
    n=20
    print()
    ls[0].plot(pred, n=n, pr=True)
