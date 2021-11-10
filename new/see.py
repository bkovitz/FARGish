# see.py -- Functions to run experiments, i.e. to "see" something

from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, IO, \
    runtime_checkable
from pprint import pprint as pp
import inspect

import matplotlib.pyplot as plt  # type: ignore[import]

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
from util import as_iter, as_list, dict_str, pr, pts, short, trace


eqn_graph = Graph.with_features(
    Consumer.make_table(
        range(1, 21), range(1, 21), [plus, minus, times]
    )
) #.add_edges(MutualInhibition((Feature, Operator, Equation, int), weight=-5.0))

def see_query(seed: int=1) -> None:
    fm = FARGModel(seed=seed, slipnet=Slipnet(eqn_graph))
    activations_in = {
        Before(4): 1.0,
        After(9): 1.0
    }
    lenable(LogAdjustedDeltas)
    nodes = fm.pulse_slipnet(
        activations_in=activations_in, # type: ignore[arg-type]
        pred=Consumer,
        k=5,
        num_get=3,
        alog=fm.start_alog((None, None))
    )
    ls = list(fm.alogs.logs.values())
    print('\nPlotted, final activations:')
    ls[0].plot(n=15, pr=True)
    print('\nNodes chosen:')
    pts(nodes, key=short)
    

if __name__ == '__main__':
    see_query()
