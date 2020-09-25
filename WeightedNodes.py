# WeightedNodes.py -- Class for choosing nodes by weighted probability

from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Callable

from Node import NRef, NRefs
from util import is_iter, as_iter, sample_without_replacement


# TODO UT
class WeightedNodes:
    '''Each element in 'nodes' can be either a nodeid or a collection of
    nodeids.'''
    def __init__(
        self,
        g: 'ActiveGraph',
        nodes: NRefs,
        weight_f: Union[Callable[[NRef], float], None]=None
    ):
        self.nodes = []
        self.weights = []
        if not weight_f:
            weight_f = g.activation
        for node in as_iter(nodes):
            if is_iter(node):
                weight = sum(weight_f(n) for n in node)
            else:
                weight = weight_f(node)
            self.add(node, weight)

    def add(self, node: NRef, weight: float):
        if weight > 0.0:
            self.nodes.append(node)
            self.weights.append(weight)

    def choose(self, k=1):
        '''Returns a generator of k nodes chosen randomly, weighted by
        salience.'''
        return sample_without_replacement(
            self.nodes, k=k, weights=self.weights
        )

    def choose1(self):
        '''Returns a randomly chosen node, or None if there are no nodes
        in the NodesWithSalience.'''
        lis = list(self.choose(k=1))
        if lis:
            return lis[0]
        else:
            None

    def __len__(self):
        return len(self.nodes)

    def __repr__(self):
        if not self.nodes:
            return 'WeightedNodes()'
        fmt = '  %%%ds  %%.3f' % max(len(str(node)) for node in self.nodes)
        return 'WeightedNodes(\n%s\n)' % '\n'.join(
            (fmt % tup)
                for tup in sorted(
                    zip(self.nodes, self.weights), key=itemgetter(1)
                )
        )
