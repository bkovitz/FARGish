# spike.py -- Architectural spike for FARGish/Numbo without a port graph

from pprint import pprint as pp
import inspect
from time import process_time

from dataclasses import dataclass, field
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable
from itertools import chain
from copy import copy
import operator
from operator import itemgetter, attrgetter
from heapq import nlargest
from collections import Counter

import networkx as nx

from Propagator import Propagator, Delta
from util import is_iter, as_iter, pts


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
class Increase:
    pass

@dataclass(frozen=True)
class Decrease:
    pass

@dataclass(frozen=True)
class FeatureWrapper:
    feature: Union[Hashable, None] = None
    
    def __str__(self):
        return f'{self.__class__.__name__}({self.feature})'

    def features(self):
        yield self.feature

class Before(FeatureWrapper):
    pass

class After(FeatureWrapper):
    pass

class MinBefore(FeatureWrapper):
    pass

class MaxBefore(FeatureWrapper):
    pass

class Doubled(FeatureWrapper):
    pass

class NumOperands(FeatureWrapper):
    pass

class OneUniqueBefore(FeatureWrapper):
    pass

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

@dataclass(frozen=True)
class SequentialBefore:
    lb: Any
    ub: Any

#@dataclass(frozen=True)
#class Before:
#    '''A feature meaning that .obj was present before the action represented
#    by the slipnode occurred.'''
#    obj: Hashable
#
#    def __str__(self):
#        return f'Before({self.obj})'
#
#@dataclass(frozen=True)
#class After:
#    '''A feature meaning that .obj was present after the action represented
#    by the slipnode occurred.'''
#    obj: Hashable
#
#    def __str__(self):
#        return f'After({self.obj})'

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
        if all(self.result > operand for operand in self.operands):
            yield Increase()
        elif any(self.result < operand for operand in self.operands):
            yield Decrease()
        counter = Counter(self.operands)
        for operand, count in counter.items():
            if count == 2:
                yield Doubled(operand)
                yield Doubled()
        yield NumOperands(len(self.operands))
        mino = min(self.operands)
        maxo = max(self.operands)
        yield MinBefore(mino)
        yield MaxBefore(maxo)
        if mino == maxo:
            yield OneUniqueBefore(mino)
        elif set(range(mino, maxo + 1)) == set(self.operands):
            yield SequentialBefore(mino, maxo)

    def __str__(self):
        expr = f' {self.operator} '.join(str(n) for n in self.operands)
        return f'{expr} = {self.result}'

@dataclass(frozen=True)
class Numble:
    bricks: List[int]
    target: int
    operators: Set[Operator] = frozenset([plus, times, minus])

@dataclass(frozen=True)
class SolnState:
    avails: List[int]
    last_move: Union[None, str] = None

    def move(self, operator: Operator, operands: List[int]) -> 'SolnState':
        avs = copy(self.avails)
        for operand in operands:
            avs.remove(operand)
            #TODO ValueError
        result = operator.call(*operands)
        expr = operator.name.join(str(n) for n in operands)
        return SolnState(
            [result] + avs,
            last_move=f'{expr}={result}'
        )

    def avails_str(self) -> str:
        return ' '.join(str(n) for n in self.avails)

    def __str__(self):
        astr = self.avails_str()
        if self.last_move:
            return ' '.join(str(x) for x in [self.last_move] + self.avails)
        else:
            return self.avails_str()

@dataclass(frozen=True)
class SolnCanvas:
    cells: List[SolnState]

    def move(self, operator: Operator, operands: List[int]) -> 'SolnCanvas':
        ss = copy(self.cells)
        # TODO Exception if self.cells is empty
        new_state = ss[-1].move(operator, operands)
        ss.append(new_state)
        return self.__class__(ss)

    @classmethod
    def init(cls, avails: List[int]) -> 'SolnCanvas':
        return cls([SolnState(avails)])

    def __str__(self):
        # TODO string when self.cells is empty
        return '; '.join(str(s) for s in self.cells)

@dataclass
class SlipnetPropagator(Propagator):

    noise: float = 0.0  #0.005
    max_total: float = 10.0
    positive_feedback_rate: float = 0.0  # higher -> initial features matter more
    sigmoid_p: float = 1.1  # higher -> sharper distinctions, more salience
    num_iterations: int = 20
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

    # NEXT Limit to 2 levels of features
    def xfeatures_of(self, x0) -> Set[Hashable]:
        result = set()
        visited = set()
        to_visit = {x0}
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
        elif isinstance(x, int):
            if x & 1:
                yield Odd()
            else:
                yield Even()
            s = str(x)
            if len(s) > 1:
                yield Leading(int(s[0]))
                yield Trailing(int(s[1]))
        else:
            #raise ValueError(x)
            return

    features_of = features_of1

    def incident_nws(self, node: Hashable) -> List[NeighborW]:
        return [
            NeighborW(neighbor, edge_d.get('weight', 1.0))
                for neighbor, edge_d in self.adj[node].items()
        ]

    def dquery(
        self,
        features: Iterable[Hashable]
    ) -> Dict[Hashable, float]:
        '''Returns dictionary of activations.'''
        activations_in = {}
        for f in features:
            if isinstance(f, NodeA):
                a = f.a
                f = f.node
            else:
                try:
                    a = f.default_a
                except AttributeError:
                    a = 1.0
            activations_in[f] = max(activations_in.get(f, 0.0), a)
        return self.propagator.propagate(self, activations_in)

    def query(
        self,
        features: Iterable[Hashable],
        type: Type,
        k: Union[int, None]=None
    ) -> List:
        activations_out = self.dquery(features)
        print('SUM', sum(activations_out.values()))
        return self.top(activations_out, type, k)

    def top(
        self,
        d: Dict[Hashable, float],
        type: Type,
        k: Union[int, None]=None
    ) -> List[NodeA]:
        nas = [
            NodeA(node, a)
                for (node, a) in d.items()
                    if isinstance(node, type)
        ]
        if k is None:
            return sorted(nas, key=attrgetter('a'), reverse=True)
        else:
            return nlargest(k, nas, key=attrgetter('a'))

slipnet = Slipnet(
    Equation((a, b), operator, operator.call(a, b))
        for a in range(1, 11)
        for b in range(1, 11)
        for operator in [plus, times, minus]
        if a >= b
)
slipnet.add_layer2_nodes([
    Equation((4, 5, 6), plus, 15)
])
slipnet.add_layer2_nodes(
    Equation((a, 1), operator, operator.call(a, 1))
        for a in range(10, 102)
        for operator in [plus, minus]
)
slipnet.add_layer2_nodes(
    Equation((a, 2), plus, a + 2)
        for a in range(0, 102, 2)
)

numble = Numble([4, 5, 6], 15)
ss0 = SolnState([4, 5, 6])
ss1 = ss0.move(plus, [4, 5])

sc0 = SolnCanvas.init([4, 5, 6])
sc1 = sc0.move(plus, [4, 5])
sc2 = sc1.move(plus, [9, 6])

print(sc2)

f45 = [4, 5, Before(4), Before(5)]
f456 = [Before(4), Before(5), Before(6), After(15)]
f22 = [Before(2), Doubled(2), After(4)] # WANT addition favored by default
                                        # over multiplication.
f1 = [1]
#q = slipnet.query(f456, object)
#pts(q)
#d = slipnet.dquery(f456)

slipnet.add_layer2_nodes([40, 50, 60])
slipnet.add_edge(Leading(4), 40, weight=1.0)
slipnet.add_edge(Leading(5), 50, weight=1.0)

# "Backwash" test: will 40 and 50 receive much activation?
q = slipnet.dquery([Equation((5, 4), plus, 9)])
pts(q)
