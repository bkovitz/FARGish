# FARGish3.py -- The generic classes and functions for making a FARG model

# Unlike in FARGish2.py, this version doesn't build instances of workspace
# objects. It builds only objects that contain a class, a dictionary of
# contents, and a dictionary defining an orientation. FARG classes contain
# only class methods, passed these dictionaries as arguments.

from pprint import pprint as pp
import inspect
from time import process_time
import csv

from dataclasses import dataclass, field, fields, replace, is_dataclass, InitVar
import dataclasses
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence
from numbers import Number, Real
from math import exp
from abc import ABC, abstractmethod
from itertools import chain
from copy import copy
import operator
from operator import itemgetter, attrgetter
from heapq import nlargest
from collections import Counter, defaultdict
from io import StringIO
from inspect import isclass
import inspect

import networkx as nx
import matplotlib.pyplot as plt
#import netgraph

from Slipnet import Slipnet, empty_slipnet
from Propagator import Propagator, Delta
from util import is_iter, as_iter, as_list, pts, pl, pr, csep, ssep, \
    as_hashable, backslash, singleton, first, tupdict, as_dict, short, \
    sample_without_replacement, clip, reseed, default_field_value, d_subset, \
    fields_for, filter_none


# Types

Value = NewType('Value', Hashable)
Addr = NewType('Addr', Hashable)

@dataclass(frozen=True)
class Elem(ABC):
    # An element of the workspace, possibly of the slipnet
    #cl: Union[Type, None]
    contents: Dict[str, Hashable]

@dataclass(frozen=True)
class Agent(Elem):
    orientation: Dict[str, Hashable]

    @abstractmethod
    def go(cls, fm: 'FARGModel', contents, orientation):
        pass

    @abstractmethod
    def can_go(cls, fm: 'FARGModel', contents, orientation):
        pass

# Graphs

class ActivationGraph(nx.DiGraph):

    def __init__(self, **kwargs):
        super().__init__()
        self.propagator = ActivationPropagator(**kwargs.get('aprop', {}))
#            **fields_for(ActivationPropagator, kwargs)
#        )

    def ns(self, node) -> List[str]:
        '''Returns list of neighbors represented as strings.'''
        return [gstr(neighbor) for neighbor in self.neighbors(node)]

    def add_node(self, node: Hashable, a=1.0):
        super().add_node(node, a=a)

    def add_edge(self, node1: Hashable, node2: Hashable, weight=1.0):
        if abs(weight) < 0.001:
            self.remove_edge(node1, node2)
        else:
            super().add_edge(node1, node2, weight=weight)
            
    def remove_edge(self, node1: Hashable, node2: Hashable):
        if self.has_edge(node1, node2):
            super().remove_edge(node1, node2)

    def a_dict(self) -> Dict[Elem, float]:
        '''Activations dictionary.'''
        return dict(
            (node, self.a(node)) for node in self.nodes
        )

    def a(self, node: Elem) -> float:
        try:
            return self.nodes[node]['a']
        except KeyError:
            return 0.0

    def propagate(self) -> Dict:  # TODO Update the graph
        d = self.propagator.propagate(self, self.a_dict())
        for node, a in d.items():
            self.nodes[node]['a'] = a
        return d

#    def pr_flows(self):
#        lines = [
#            f'{self.arrow(k):75s} {v:= 3.5f}'
#                for k, v in self.propagator.flows.items()
#        ]
#        pr(lines)
#
#    @classmethod
#    def arrow(cls, k) -> str:
#        fromnode = k[0]
#        tonode = k[1]
#        return f'{fromnode} -> {tonode}'
    def pr_flows(self):
        pr(self.propagator.flows)

    def draw(self):
        global pos, node_labels, plot_instance
        pos = nx.layout.spring_layout(self)
        node_labels = dict((node, gstr(node)) for node in self.nodes)
#        nx.draw(self, pos, with_labels=True, labels=node_labels)
#        nx.draw_networkx_edge_labels(
#            self, pos, edge_labels=nx.get_edge_attributes(self, 'weight')
#        )
        plot_instance = netgraph.InteractiveGraph(
            self,
            node_positions=pos,
            node_labels=node_labels,
            node_label_font_size=6
        )
        #plot_instance = netgraph.InteractiveGraph(self)

    def pr(self):
        '''Prints alphabetized list of nodes with activations.'''
        for s in sorted(self.nodestr(n) for n in self.nodes):
            print(s)

    def nodestr(self, node):
        return f"{node!s:50s} {self.nodes[node]['a']:2.5f}"

    def decay(self):
        for node in self.nodes:
            self.nodes[node]['a'] *= 0.95

    def boost(self, nodes):  # TODO type annotation
        for node in as_iter(nodes):
            a = self.nodes[node]['a']
            #incr = max(min(1.0, a), 2.0)
            incr = clip(0.5, 1.0, a)
            self.nodes[node]['a'] += incr
            print('BOOST', node, 'TO', self.a(node)) #DIAG

@dataclass
class ActivationPropagator(Propagator):
    '''This works like StdGraph.StdSupportPropagator, except that activation
    rather than support flows between nodes, and every edge (not just edges
    with certain labels) is taken as a path for activation to flow.'''
    noise: float = 0.0  #0.005
    max_total: float = 40.0
    positive_feedback_rate: float = 0.1 #0.5  # higher -> initial features matter more
    sigmoid_p: float = 0.9 #1.05  # higher -> sharper distinctions, more salience
    num_iterations: int = 10
    alpha: float = 0.98
    inflation_constant: float = 5.0  # 2.0 is minimum  # TODO rm?

#    def min_value(self, g, node):
#        return 0.0
    def clip_a(self, g, node, a):
        lb = getattr(node, 'min_a', 0.0)
        ub = getattr(node, 'max_a', 10.0)
        return clip(lb, ub, a)

    def make_deltas(self, g, old_d):
        return chain.from_iterable(
            self.deltas_from(g, old_d, nodeid)
                for nodeid in old_d
        )

    def deltas_from(self, g, old_d, node) -> Iterable[Delta]:
        '''Deltas specify changes to node's neighbors, not to node.'''
        node_a = old_d.get(node, 0.0)  # node's current activation
        outgoing_deltas = []
        for neighbor, edge_d in g.adj[node].items():
            weight = edge_d.get('weight', 1.0)
            neighbor_a = old_d.get(node, 0.0)  # neighbor's current activation
            outgoing_deltas.append(
                Delta(
                    neighbor,
                    weight, # + self.positive_feedback_rate * neighbor_a,
                    node
                )
            )
        return self.rescale_deltas(outgoing_deltas, node_a)

    def rescale_deltas(self, deltas, node_a) -> List[Delta]:
        if not deltas:
            return deltas
        ssum = sum(delta.amt for delta in deltas)
        if ssum <= node_a:
            return deltas
        else:
            multiplier = node_a / ssum
            return [
                Delta(d.nodeid, d.amt * multiplier, d.neighborid)
                    for d in deltas
            ]

# Classes

@dataclass
class ElemInWS:
    '''An element in the workspace.'''
    elem: Elem
    builder: Union[Agent, None]
    behalf_of: Union[List[Agent], None]
    tob: int   # time of birth (when Elem was added to the ws)
        # TODO Allow multiple builders?
    # activation: float = 1.0

    def __str__(self):
        return f'{self.elem}  builder={self.builder} tob={self.tob}'

@dataclass
class FARGModel:
    ws: Dict[Hashable, ElemInWS] = field(default_factory=dict)
    t: int = 0
    slipnet: Slipnet = empty_slipnet  # TODO copy empty_slipnet?
    seed: Union[int, None] = None

    activation_g: ActivationGraph = field(
        default_factory=ActivationGraph, init=False
    )
    sleeping: InitVar[Dict[Elem, int]] = None # = field(
#        default_factory=dict, init=False
#    )

    mutual_support_weight: float = 1.0
    mutual_antipathy_weight: float = -0.2

    globals: Dict[str, Any] = field(default_factory=dict, init=False)

    def build(self) -> Elem:  # TODO more args
        raise NotImplementedError

    def elems(self, pred=None, es=None) ->Iterable[Elem]:
        raise NotImplementedError


# Numbo-specific

@dataclass(frozen=True)
class Want(Agent):
    fs = ['target']  # fields

    def __init__(self, *args, **kwargs):



@dataclass(frozen=True)
class Consume(Painter):

    def paint(self, fm, src: CellRef, dest: CellRef, **kwargs):
        raise NotImplementedError

    def value_to_paint(self, fm, src: CellRef, **kwargs):
        '''Returns the value even if the operands are not avail.'''
        raise NotImplementedError
