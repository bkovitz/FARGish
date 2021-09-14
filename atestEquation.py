# atestEquation.py -- Acceptance tests related to Equations

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass, field
import operator
from operator import itemgetter, attrgetter
from heapq import nlargest
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence, Literal

from Equation import Equation, Operator, plus, times, minus
from Equation import IncreaseOrDecrease, Increase, Decrease, NumOperands, \
    Before, After, MaxBefore, MinBefore
from Graph2 import Graph, Node, Hop, Hops, Nodes, Edges, EnumNodes, EnumEdges, \
    OfClass, MutualInhibition, Feature, features_of, GraphPropagatorOutgoing
from Propagator import Propagator
from util import as_iter, pts, pr


eqn_graph = Graph.with_features(
    Equation.make_table(
        range(1, 11), range(1, 11), [plus, minus, times]
    )
).add_edges(MutualInhibition((Feature, int), weight=-0.8))
p = GraphPropagatorOutgoing(
    max_total=10.0,
    noise=0.0,
    positive_feedback_rate=2.0,  # higher -> initial features matter more
    sigmoid_p=1.05,  # higher -> sharper distinctions, more salience
    num_iterations=20,
    alpha= 0.95,
)

@dataclass(frozen=True)
class NodeA:
    '''Node and activation.'''
    node: Node
    a: float

    def __str__(self):
        return f'{self.node!s:20s} {self.a:2.5f}'

def slipnet_dquery(
    g: Graph,
    p: Propagator,
    features: Iterable[Hashable]=None,
    activations_in: Dict[Hashable, float]=None
) -> Dict[Hashable, float]:
    '''Pass either features or a dictionary of activations.
    Returns dictionary of activations.'''
    if activations_in is None:
        activations_in = {}
        for f in as_iter(features):
            activations_in[f] = 1.0
    #print('DQ', type(activations_in))
    return p.propagate(g, activations_in)

def top(
    d: Dict[Hashable, float],
    type: Type=None,
    k: Union[int, None]=None,
    filter: Union[Callable, None]=None
) -> List[NodeA]:
    if filter is None:
        filter = lambda x: True
    if type is None:
        nas = [
            NodeA(node, a)
                for (node, a) in d.items()
                    if filter(node)
        ]
    else:
        nas = [
            NodeA(node, a)
                for (node, a) in d.items()
                    if isinstance(node, type) and filter(node)
        ]
    if k is None:
        return sorted(nas, key=attrgetter('a'), reverse=True)
    else:
        return nlargest(k, nas, key=attrgetter('a'))

if __name__ == '__main__':
    out_d = slipnet_dquery(eqn_graph, p, features=[Before(4), After(10)])
    pts(top(out_d, k=30))
    print()
    pts(top(out_d, type=Equation, k=30))
    print()
    pts(top(out_d, type=int, k=30))

    '''
    for eqn in sorted(eqn_graph.query(None), key=str):
        print(eqn, '    ', eqn_graph.degree_out(eqn), eqn_graph.degree_in(eqn))
    '''
