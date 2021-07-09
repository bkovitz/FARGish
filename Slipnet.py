# Slipnet.py -- Generic slipnet class

from dataclasses import dataclass, field
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence
from itertools import chain
from copy import copy
import operator
from operator import itemgetter, attrgetter
from heapq import nlargest

import networkx as nx

from Propagator import Propagator, Delta
from util import is_iter, as_iter, pts, pl, pr


NodeId = NewType('NodeId', int)

class Node:
    def features(self) -> Iterable[Hashable]:
        return []

@dataclass(frozen=True)
class NodeA:
    '''Node and activation.'''
    node: Node
    a: float

    def __str__(self):
        return f'{self.node!s:20s} {self.a:2.5f}'

@dataclass(frozen=True)
class NeighborW:
    '''Neighbor node and edge weight.'''
    neighbor: Node
    weight: float

@dataclass(frozen=True)
class FeatureWrapper:
    feature: Union[Hashable, None] = None
    
    def __str__(self):
        return f'{self.__class__.__name__}({self.feature})'

    def features(self):
        yield self.feature

@dataclass
class SlipnetPropagator(Propagator):
    noise: float = 0.0  #0.005
    max_total: float = 10.0
    positive_feedback_rate: float = 0.5  # higher -> initial features matter more
    sigmoid_p: float = 1.05  # higher -> sharper distinctions, more salience
    num_iterations: int = 10
    alpha: float = 0.95
    inflation_constant: float = 5.0  # 2.0 is minimum
    
    def make_deltas(self, g, old_d):
        #print() #DEBUG
        return chain.from_iterable(
            self.deltas_from(g, old_d, nodeid)
                for nodeid in old_d
        )

    def INFLATIONARY_deltas_from(self, g, old_d, nodeid) \
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

    def deltas_from(self, g, old_d, nodeid) \
    -> List[Delta]:
        '''Deltas from nodeid to its neighbors.

        Outgoing weights are quasi-averaged in a way similar to that used by
        Toby Tyrell, but the quasi-averaging is done on the outgoing edges
        rather than the incoming edges. This might not work as well.'''
        result: List[Delta] = []
        nodeid_a = old_d.get(nodeid, 0.0)
        nws: List[NeighborW] = g.incident_nws(nodeid)
        num_edges = len(nws)
#        wtotal = sum(nws, key=attrgetter('weight'))
#        wmax = max(nws, key=attrgetter('weight'))
#        alpha = 1.0 / num_edges**2
        multiplier = self.inflation_constant / (
            num_edges + self.inflation_constant - 1
        )
        for nw in nws:
            delta = Delta(
                nw.neighbor,
                nodeid_a * nw.weight * multiplier,
                nodeid
            )
            result.append(delta)
        return result

    def min_value(self, g, nodeid):
        return 0.0

class Slipnet(nx.Graph):

    def __init__(self, nodes: Iterable[Node] = []):
        super().__init__()
        self.features = set()
        self.propagator = SlipnetPropagator()
        self.add_layer2_nodes(nodes)

    def ns(self, node) -> List[str]:
        '''Returns list of neighbors represented as strings.'''
        return [str(neighbor) for neighbor in self.neighbors(node)]

    def add_layer2_nodes(self, nodes: Iterable[Node]):
        for node in nodes:
            self.add_node(node)
            for f in as_iter(self.features_of(node)):
                self.add_edge(f, node, weight=1.0)
                self.features.add(f)

    # TODO Limit to 2 levels of features
    def xfeatures_of(self, x0) -> Set[Hashable]:
        result = set()
        visited = set()
        to_visit = {x0}
        print('XF', x0)
        while to_visit:
            next_to_visit = set()
            for x in to_visit:
                visited.add(x)
                for f in self.features_of1(x):
                    result.add(f)
                    if f not in visited:
                        next_to_visit.add(f)
            to_visit = next_to_visit
        return result

    def features_of1(self, x) -> Union[Iterable[Hashable], None]:
        if hasattr(x, 'features'):
            yield from x.features()
        else:
            yield from self.default_features(x)

    features_of = features_of1

    def default_features(self, x) -> Union[Iterable[Hashable], None]:
        '''Override this in subclasses.'''
        if False:
            yield None

    # TODO UT, UT with non-existent node
    def incident_nws(self, node: Hashable) -> List[NeighborW]:
        try:
            return [
                NeighborW(neighbor, edge_d.get('weight', 1.0))
                    for neighbor, edge_d in self.adj[node].items()
            ]
        except KeyError:
            #print('INCNWS', node, len(self.nodes)) #DIAG
            return []

    def dquery(
        self,
        features: Iterable[Hashable]=None,
        activations_in: Dict[Hashable, float]=None
    ) -> Dict[Hashable, float]:
        '''Pass either features or a dictionary of activations.
        Returns dictionary of activations.'''
        if activations_in is None:
            activations_in = {}
            for f in as_iter(features):
                if isinstance(f, NodeA):
                    a = f.a
                    f = f.node
                else:
                    try:
                        a = f.default_a
                    except AttributeError:
                        a = 1.0
                activations_in[f] = max(activations_in.get(f, 0.0), a)
        #print('DQ', type(activations_in))
        return self.propagator.propagate(self, activations_in)

    def query(
        self,
        features: Iterable[Hashable]=None,
        activations_in: Dict[Hashable, float]=None,
        type: Type=None,
        k: Union[int, None]=None,
        filter: Union[Callable, None]=None
    ) -> List:
        activations_out = self.dquery(
            features=features, activations_in=activations_in
        )
        #print('QUERY')
        #pr(self.top(activations_out, k=k))
        #print('SUM', sum(activations_out.values()))
        return self.top(activations_out, type, k, filter=filter)

    @classmethod
    def to_d(cls, nas: List[NodeA]) -> Dict[Hashable, float]:
        return dict((na.node, na.a) for na in nas)

    @classmethod
    def top(
        cls,
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

class Leading(FeatureWrapper):
    '''Indicates the leading digit of something.'''
    pass

class Trailing(FeatureWrapper):
    '''Indicates the last digit of something.'''
    pass

@dataclass(frozen=True)
class Even:
    pass

@dataclass(frozen=True)
class Odd:
    pass

class IntFeatures(Slipnet):

    def default_features(self, x):
        print('INTF')
        if isinstance(x, int):
            if x & 1:
                yield Odd()
            else:
                yield Even()
            s = str(x)
            if len(s) > 1:
                yield Leading(int(s[0]))
                yield Trailing(int(s[1]))
        else:
            yield from super().default_features(x)

empty_slipnet = Slipnet()
