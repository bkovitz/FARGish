# see.py -- Functions to run experiments, i.e. to "see" something

from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, IO, \
    runtime_checkable
from pprint import pprint as pp
import inspect

import matplotlib.pyplot as plt  # type: ignore[import]

from FMTypes import *
from FARGModel import FARGModel, FARGException, SolvedPuzzle, CellRef, \
    Agent, Codelet, Fizzle
from Log import *
from Propagator import *
from Graph import *
from Slipnet import *
from Agents import *
from Codelets import *
from Canvas import *
from Equation import *
from Features import *
from util import as_iter, as_list, dict_str, pr, pts, short


eqn_graph = Graph.with_features(
    Consumer.make_table(
        range(1, 21), range(1, 21), [plus, minus, times]
    )
) #.add_edges(MutualInhibition((Feature, Operator, Equation, int), weight=-5.0))

desnag_graph = Graph.with_features(
    [VariantMakerFromAvails()]
)

fm: FARGModel
ls: Any
nodes: Iterable[Node]
activations_in: ADict

def see_query(
    q: Iterable[Node]=(Before(4), After(9)),
    pred: WSPred=Consumer,
    seed: int=1,
    sngraphs: Union[Graph, Iterable[Graph]]=eqn_graph
) -> None:
    global fm, ls, nodes, activations_in
    fm = FARGModel(
        seed=seed,
        slipnet=Slipnet(Graph.augment(*as_iter(sngraphs)))
    )
    activations_in = dict((node, 1.0) for node in q)
    lenable(LogAdjustedDeltas)
    nodes = fm.pulse_slipnet(
        activations_in=activations_in, # type: ignore[arg-type]
        pred=pred,
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
    see_query(q=[Desnag], pred=Agent, sngraphs=[desnag_graph])
    #g = Graph.with_features([VariantMakerFromAvails()])
    #pr(g)
