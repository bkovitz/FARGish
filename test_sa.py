# test_sa.py -- Test of spreading activation

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass
import operator
from operator import itemgetter
from time import process_time

from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable
from itertools import chain

import networkx as nx

from Propagator import Propagator, Delta


NodeId = NewType('NodeId', int)

@dataclass
class MyProp(Propagator):

    noise: float = 0.0
    
    def make_deltas(self, g, old_d):
        print() #DEBUG
        return chain.from_iterable(
            self.deltas_from(g, old_d, nodeid)
                for nodeid in old_d
        )

    def deltas_from(self, g, old_d, nodeid) \
    -> List[Delta]:
        '''Deltas from nodeid to its neighbors.'''
        result: List[Delta] = []
        nodeid_a = old_d.get(nodeid, 0.0)
        for neighborid, edge_d in g.adj[nodeid].items():
            weight = edge_d.get('weight', 1.0)
            delta = Delta(
                neighborid,
                weight * nodeid_a,
                nodeid
            )
            result.append(delta)
        return result

    def min_value(self, g, nodeid):
        return 0.0

class Node:
    nodeid: NodeId

@dataclass(frozen=True)
class Operator:
    func: Callable
    name: str

    def call(self, *operands: int) -> int:
        return self.func(*operands)

    def __str__(self):
        return self.name

plus = Operator(operator.add, '+')
times = Operator(operator.mul, 'x')
minus = Operator(operator.sub, '-')

@dataclass(frozen=True)
class Before:
    '''A feature meaning that .obj was present before the action represented
    by the slipnode occurred.'''
    obj: Hashable

    def __str__(self):
        return f'Before({self.obj})'

@dataclass(frozen=True)
class After:
    '''A feature meaning that .obj was present after the action represented
    by the slipnode occurred.'''
    obj: Hashable

    def __str__(self):
        return f'After({self.obj})'

@dataclass(frozen=True)
class Equation(Node):
    operands: Tuple[int]
    operator: Operator
    result: int

    def features(self) -> Iterable[Hashable]:
        for operand in self.operands:
            yield operand
            yield Before(operand)
        yield self.operator
        yield self.result
        yield After(self.result)
        #return set(self.operands + (self.operator, self.result, Before

    def __str__(self):
        expr = f' {self.operator} '.join(str(n) for n in self.operands)
        return f'{expr} = {self.result}'

#class Graph:
#
#    def __init__(self):
#        impl = nx.Graph()
#
#    def add_edge(
    

class TestSA(unittest.TestCase):

    def test_sa(self):
        p = MyProp()
        self.assertEqual(p.noise, 0.0)

        g = nx.Graph()  # undirected graph

        g.add_edge(1, 2, weight=1.0)
        g.add_edge(1, 3, weight=1.3)
        g.add_node(4)

        #print(g.edges[1, 2]['weight'])

        #for neighbor in g.adj[1].items():
            #print(neighbor)

        # Let's give all nodes activation=1.0.
        initial_a_dict = dict((nodeid, 1.0) for nodeid in g.nodes)

        # Propagate
        got: Dict[NodeId, float] = p.propagate(g, initial_a_dict)
        self.assertEqual(got, {1: 1.026, 2: 1.0, 3: 1.006, 4: 0.98})

    def test_eqns(self):
        pass

if __name__ == '__main__':
    import matplotlib.pyplot as plt
    plt.ion()

    p = MyProp(positive_feedback_rate=0.0, sigmoid_p=0.9)

    def query(g, features):
        activations_in = dict((f, 1.0) for f in features)
        activations_out = p.propagate(g, activations_in, num_iterations=10)
        return activations_out

    def see(activations_d):
        for node, a in sorted(activations_d.items(), key=itemgetter(1)):
            print(f'{node!s:20s} {a:0.3f}')

    g = nx.Graph()

    for a in range(1, 11):
        for b in range(1, 11):
            if b >= a:
                continue
            for operator in [plus, minus, times]:
                e = Equation((a, b), operator, operator.call(a, b))
                g.add_node(e)
                for f in e.features():
                    g.add_edge(f, e, weight=1.0)
    #e1 = Equation((2, 3), plus, plus.call(2, 3))
    #print(e1)
#    g.add_node(e1)
#    for f in e1.features():
#        g.add_edge(f, e1, weight=1.0)

    #a0 = dict((f, 1.0) for f in [4, 5, Before(4), Before(5)])
    a0 = dict((f, 1.0) for f in [7, 6, Before(7), Before(6)])
    see(a0)
    print()

    start = process_time()
    a1 = p.propagate(g, a0, num_iterations=20)
    end = process_time()
    print(end - start)
    see(a1)
    print(sum(a1.values()))

    nx.draw(g, with_labels=True, pos=nx.bipartite_layout(g, [n for n in g.nodes if isinstance(n, Equation)]))
    #plt.show()
