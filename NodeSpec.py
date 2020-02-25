# NodeSpec.py -- Classes that denote nodes to search for

from abc import ABC, abstractmethod
from random import choice
from itertools import product

from Action import FuncAction
from bases import NewLinkSpec, meets_link_spec #TODO rm
from PortGraph import NodesWithSalience, pg
from util import as_iter, is_nodeid, intersection, nice_object_repr, NiceRepr


class BaseNodeSpec(ABC):
    '''A specification of a condition for nodes to satisfy.'''

    @abstractmethod
    def is_match(self, g, nodeid):
        '''Does nodeid match this NodeSpec?'''
        pass

    def see_all(self, g, nodes=None):
        '''Returns a list of all the nodes in g that satisfy this NodeSpec.
        If nodes is not None, it's a subset of nodes in which to search.'''
        if nodes is None:
            nodes = g.nodes # All nodes: inefficient
        return [n for n in nodes if self.is_match(g, n)]

    def see_one(self, g, nodes=None):
        '''Returns a single node in g that satisfies this NodeSpec, chosen
        randomly if there are more than one, or None. If nodes is not None,
        specifies a subset of nodes in which to search.'''
        ns = NodesWithSalience(g, self.see_all(g, nodes))
        return ns.choose1()
#        if ns:
#            return choice(ns) #TODO weight choice by salience or other
#                              #specified criterion?
#        else:
#            return None

    __repr__ = nice_object_repr

class NodeSpec(BaseNodeSpec):

    def __init__(self, nodeclass=None, tagclass=None, value=None):
        self.nodeclass = nodeclass
        self.tagclass = tagclass
        self.value = value

    def is_match(self, g, nodeid):
        if self.nodeclass is not None:
            if not g.is_of_class(nodeid, self.nodeclass):
                return False
        if self.tagclass is not None:
            if not g.has_tag(nodeid, self.tagclass):
                return False
        if self.value is not None:
            return g.value_of(nodeid) == self.value
        else:
            return True

class NodeOfClass(BaseNodeSpec):

    def __init__(self, nodeclass):
        self.nodeclass = nodeclass

    def is_match(self, g, nodeid):
        return g.is_of_class(nodeid, self.nodeclass)

class NodeWithTag(BaseNodeSpec):

    def __init__(self, nodeclass, tagclass):
        self.nodeclass = nodeclass
        self.tagclass = tagclass

    def is_match(self, g, nodeid):
        return (
            g.is_of_class(nodeid, self.nodeclass)
            and
            g.has_tag(nodeid, self.tagclass)
        )

#TODO UT
#TODO Just call PortGraph.has_neighbor_at()
class NodeWithNeighborAt(BaseNodeSpec):

    def __init__(self, port_label, neighbor_class=None, node_class=None):
        self.port_label = port_label
        self.neighbor_class = neighbor_class
        self.node_class = node_class

    def is_match(self, g, nodeid):
        if (self.node_class is not None
            and
            not g.is_of_class(nodeid, self.node_class)
        ):
            return False
        neighbors = g.neighbors(nodeid, port_label=self.port_label)
        if len(neighbors) == 0:
            return False
        if self.neighbor_class is None:
            return True
        return any(g.is_of_class(neighbor, self.neighbor_class)
                       for neighbor in neighbors)

#TODO UT nodeclass and tagclass
class NodeWithValue(BaseNodeSpec):

    def __init__(self, value, nodeclass=None, tagclass=None):
        self.value = value
        self.nodeclass = nodeclass
        self.tagclass = tagclass

    def is_match(self, g, nodeid):
        if self.nodeclass is not None:
            if not g.is_of_class(nodeid, self.nodeclass):
                return False
        if self.tagclass is not None:
            if not g.has_tag(nodeid, self.tagclass):
                return False
        return g.value_of(nodeid) == self.value

class HasSameValue(BaseNodeSpec):
    '''Matches nodes with same value as targetid, but does not match
    targetid.'''

    def __init__(self, targetid):
        self.targetid = targetid

    def is_match(self, g, nodeid):
        if nodeid == self.targetid:
            return False
        return g.value_of(nodeid) == g.value_of(self.targetid)

class And(BaseNodeSpec):

    def __init__(self, *conjuncts):
        '''Conjuncts are NodeSpecs. And(conjuncts) matches nodes that match
        all the conjuncts.'''
        self.conjuncts = conjuncts

    def is_match(self, g, nodeid):
        return all(c.is_match(g, nodeid) for c in self.conjuncts)

class Not(BaseNodeSpec):

    def __init__(self, nodespec):
        self.nodespec = nodespec

    def is_match(self, g, nodeid):
        return not self.nodespec.is_match(g, nodeid)

class CartesianProduct:
    '''Specifies criteria for tuples of nodes. Each node in the tuple has its
    own Nodespec, and the whole tuple can have further criteria, e.g. to
    disallow the same node from appearing more than once in one tuple.'''

    def __init__(self, *nodespecs, whole_tuple_criterion=None):
        self.nodespecs = nodespecs
        if whole_tuple_criterion is None:
            whole_tuple_criterion = tup_always_true
        self.whole_tuple_criterion = whole_tuple_criterion

    def see_all(self, g, nodes=None):
        '''Returns a list of tuples of nodes that meet the criterion.
        If nodes is not None, it specifies a subset of nodes in which to
        search.'''
        return [
            tup for tup in product(
                *(nodespec.see_all(g, nodes) for nodespec in self.nodespecs)
            ) if self.whole_tuple_criterion.is_match(g, tup)
        ]

    def see_one(self, g, nodes=None):
        '''Returns one tuple of nodes that meets the criterion, chosen
        randomly if more than one exists, or None if no such tuple exists.
        If 'nodes' is not None, it specifies a subset of nodes in which to
        search.'''
#        ns = self.see_all(g, nodes)
#        print('CART NS', ns)
#        if ns:
#            return choice(ns) # TODO Weight choice by salience or other
#                              # specified criteria?
#        else:
#            return None
        ns = NodesWithSalience(g, self.see_all(g, nodes))
        #print('CART NS', ns)
        return ns.choose1()

class TupleCriterion(ABC):
    '''Specifies a criterion for a tuple of nodes, such as is generated by
    CartesianProduct, must satisfy.'''

    @abstractmethod
    def is_match(self, g, tup):
        '''Returns True iff tup matches the criterion.'''
        pass

class TupAlwaysTrue(TupleCriterion):
    def is_match(self, g, tup):
        return True
tup_always_true = TupAlwaysTrue()

class NoDups(TupleCriterion):
    '''Allows no nodeid to appear more than once in the tuple.'''

    def is_match(self, g, tup):
        return len(set(tup)) == len(tup)

no_dups = NoDups()

class NotLinkedToSame(TupleCriterion):
    '''Disallows a tuple of nodes where the nodes all link to a single node
    via specified port labels.'''
    def __init__(self, *port_labels):
        self.port_labels = port_labels

    def is_match(self, g, tup):
        '''tup must have same length as self.port_labels.'''
        if len(tup) != len(self.port_labels):
            raise(ValueError(f'tuple {tup} and port_labels {self.port_labels} do not have the same length.'))
        mate_sets = []
        for nodeid, port_label in zip(tup, self.port_labels):
            mate_sets.append(g.neighbors(nodeid, port_label=port_label))
        common_mates = intersection(*mate_sets)
        return len(common_mates) == 0

class TupAnd(TupleCriterion):

    def __init__(self, *tupcriteria):
        self.tupcriteria = tupcriteria

    def is_match(self, g, tup):
        return all(c.is_match(g, tup) for c in self.tupcriteria)

class BuildSpec(NiceRepr):
    '''Specifies a node to build, including links to existing neighbors.'''

    def __init__(self, new_nodeclass, link_specs=None, new_node_args=None):
        self.new_nodeclass = new_nodeclass
        self.link_specs = link_specs
        self.new_node_args = new_node_args

    def already_built(self, g, old_nodeid=None):
        #TODO old_nodeid should somehow override the old_node in
        #self.link_specs.
        '''Has a node meeting the spec in relation to old_nodeid already been
        built?'''
        if self.link_specs:
            for link_spec in as_iter(self.link_specs):
                for neighbor in g.neighbors(
                    old_nodeid, port_label=link_spec.old_node_port_label
                ):
                    if self.meets(g, old_nodeid, neighbor):
                        return True
        else:
            # INEFFICIENT: With no link_spec, we examine all nodes in the graph
            return not any(
                g.class_of(nodeid) == self.new_nodeclass
                    for nodeid in g.nodes
            )

    #TODO Check new_node_args
    def meets(self, g, old_nodeid, nodeid):
        '''Does nodeid meet the spec in relation to old_nodeid?'''
        return (
            g.class_of(nodeid) == self.new_nodeclass
            and
            all(
                ls.meets_exactly(g, nodeid, old_node=old_nodeid)
                    for ls in as_iter(self.link_specs)
            )
        )

    def build(self, g, old_nodeid):
        #TODO Don't build if .already_built()
        if self.new_node_args:
            new_nodeid = g.make_node(self.new_nodeclass(*self.new_node_args))
        else:
            new_nodeid = g.make_node(self.new_nodeclass)
        for ls in as_iter(self.link_specs):
            ls.make(g, old_nodeid, new_nodeid)

    def maybe_make_build_action(self, g, old_nodeid):
        if self.already_built(g, old_nodeid):
            return None
        else:
            return FuncAction(self.build, old_nodeid)
