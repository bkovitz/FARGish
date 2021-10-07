# Numbo1a.py -- Numbo1.py revised to import FARGModel instead of FARGish2

from dataclasses import dataclass, field, replace, asdict
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence, Literal

import matplotlib.pyplot as plt  # type: ignore[import]
import numpy as np  # type: ignore[import]

from FARGModel import FARGModel, SeqCanvas, SeqState, CellRef, Want, Consume, \
    Before, After
from FMTypes import epsilon, Elem, Elems, Value, Addr, FMPred
from Graph2 import Graph, MutualInhibition
from Equation import Equation, Operator, plus, times, minus, EqnConsume
from util import pts, pl, pr

eqn_graph = Graph.with_features(
    EqnConsume.make(eqn)
        for eqn in Equation.make_table(
            range(1, 11), range(1, 11), [plus, minus, times]
        )
) #.add_edges(MutualInhibition((Feature, Equation, int), weight=-0.02))

class Numbo(FARGModel):

    def fill_slipnet(self):
        #self.slipnet = eqn_graph
        self.slipnet.base_graph = \
            Graph.augment(self.slipnet.base_graph, eqn_graph)

def r4_5_6__15(*args, **kwargs):
    global fm, ca, wa
    fm = Numbo(*args, **kwargs)
    ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
    wa = fm.build(Want(15, CellRef(ca, 0)))
    fm.do_timestep(num=19)
    pr(fm, edges=True)
    print()
    #fm.pr_flows()
    print(f'seed={fm.seed}')
    
if __name__ == '__main__':
    r4_5_6__15()

    '''
    print()
    for node in sorted(eqn_graph.nodes.nodeset, key=str):
        print(node)
    '''
