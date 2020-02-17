# NodeSpec.py -- Classes that denote nodes to search for

from abc import ABC, abstractmethod
from random import choice
from itertools import product
from inspect import isclass
from copy import copy

from bases import NewLinkSpec, meets_link_spec #TODO rm
from util import as_iter, is_nodeid, intersection, nice_object_repr, NiceRepr


class LinkSpec(NiceRepr):
    '''Specifies a link between an "old node" and a "new node". The "new node"
    is not part of the specification. The LinkSpec can answer queries about
    whether a given node is linked in the role specified for the "new node".
    A LinkSpec only optionally includes information about the "old node".
    A query must specify a particular nodeid for the "old node".'''

    def __init__(self, old_node_port_label, new_node_port_label, old_node=None):
        '''old_node can be:
        None : indicates that .meet() will search for a match.
        an integer : a specific nodeid
        a class : a node class to match against
        an object : an object to match the node's datum against'''
        self.old_node_port_label = old_node_port_label
        self.new_node_port_label = new_node_port_label
        self.old_node_matcher = make_node_matcher(old_node)
    
    def meets(self, g, new_nodeid, old_node=None):
        '''Is new_nodeid linked according to the spec? old_node overrides
        or supplements self.old_node.'''
        node_matcher = self.old_node_matcher.override(old_node)
        return any(
            g.label_is_a(hop.to_port_label, self.old_node_port_label)
            and
            node_matcher.is_match(g, hop.to_node)
                for hop in g.hops_from_port(
                    new_nodeid,
                    self.new_node_port_label
                )
        )

    def meets_exactly(self, g, new_nodeid, old_node=None):
        '''Same as .meets() but class of old_node must match exactly;
        a subclass of the required class is not enough to make a match.'''
        node_matcher = self.old_node_matcher.override(old_node)
        return any(
            g.label_is_a(hop.to_port_label, self.old_node_port_label)
            and
            node_matcher.is_exact_match(g, hop.to_node)
                for hop in g.hops_from_port(
                    new_nodeid,
                    self.new_node_port_label
                )
        )


    def make(self, g, old_nodeid, new_nodeid):
        '''Makes the specified edge between old_nodeid and new_nodeid.'''
        node_matcher = self.old_node_matcher.override(old_nodeid)
        if not node_matcher.is_match(g, old_nodeid):
            #TODO Raise a FARGException instead
            raise ValueError(f"Old node {old_nodeid} doesn't match {self}.")
        g.add_edge(
            old_nodeid,
            self.old_node_port_label,
            new_nodeid,
            self.new_node_port_label
        )

def make_node_matcher(info, id=None, cl=None, datum=None):
    '''Constructs a NodeMatcher object with correct arguments, incorporating
    info into id, cl, and datum. Can modify caller's id, cl, and/or datum.'''
    for i in as_iter(info):
        if i is None:
            continue
        elif is_nodeid(i):
            id = i  # If more than one nodeid, the last takes priority
        elif isclass(info):
            if cl is None:
                cl = set()
            cl.add(info)
        else:
            if cl is None:
                cl = set()
            if datum is None:
                datum = set()
            cl.add(info.__class__)
            datum.add(info)
    return NodeMatcher(id, cl, datum)
    
class NodeMatcher(NiceRepr):
    '''Matches a node without regard for any of its links, i.e. by id,
    class, and/or datum attrs.'''

    def __init__(self, id, cl, datum):
        '''id is either None or a nodeid. cl is either None or a set of
        nodeclasses. datum is either None or a set of datums.'''
        self.id = id
        self.cl = cl
        self.datum = datum

    def is_match(self, g, nodeid):
        if self.id is not None:
            if self.id != nodeid:
                return False
        if self.cl is not None:
            for cl in self.cl:
                if not g.is_of_class(nodeid, cl):
                    return False
        if self.datum is not None:
            node_datum = g.datum(nodeid)
            for datum in as_iter(self.datum):
                if not datum_match(datum, node_datum):
                    return False
        return True

    def is_exact_match(self, g, nodeid):
        '''Same as .is_match() but classes must match exactly, i.e. it's not
        enough for nodeid's class to be a subclass of a class specified in
        the NodeMatcher.'''
        if self.id is not None:
            if self.id != nodeid:
                return False
        if self.cl is not None:
            for cl in self.cl:
                if not cl == g.class_of(nodeid):
                    return False
        if self.datum is not None:
            node_datum = g.datum(nodeid)
            for datum in as_iter(self.datum):
                if not datum_match(datum, node_datum):
                    return False
        return True
            
    def override(self, info):
        '''Returns a new NodeMatcher, which matches info in addition to
        whatever this NodeMatcher matches, except that if info is a nodeid,
        the new NodeMatcher will match that nodeid instead of whatever this
        one matches (if any).'''
        if info is None:
            return self
        else:
            return make_node_matcher(
                info, id=self.id, cl=copy(self.cl), datum=copy(self.datum)
            )

def datum_match(d1, d2):
    try:
        d1dict = d1.__dict__
        d2dict = d2.__dict__
    except AttributeError:
        return d1 == d2
    for k,v in d1dict.items():
        if v != d2dict.get(k, None):
            return False
    return True

class NodeSpec(ABC):
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
        ns = self.see_all(g, nodes)
        if ns:
            return choice(ns) #TODO weight choice by salience or other
                              #specified criterion?
        else:
            return None

    __repr__ = nice_object_repr

class NodeOfClass(NodeSpec):

    def __init__(self, nodeclass):
        self.nodeclass = nodeclass

    def is_match(self, g, nodeid):
        return g.is_of_class(nodeid, self.nodeclass)

class NodeWithTag(NodeSpec):

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
class NodeWithNeighborAt(NodeSpec):

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
class NodeWithValue(NodeSpec):

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

class HasSameValue(NodeSpec):
    '''Matches nodes with same value as targetid, but does not match
    targetid.'''

    def __init__(self, targetid):
        self.targetid = targetid

    def is_match(self, g, nodeid):
        if nodeid == self.targetid:
            return False
        return g.value_of(nodeid) == g.value_of(self.targetid)

class And(NodeSpec):

    def __init__(self, *conjuncts):
        '''Conjuncts are NodeSpecs. And(conjuncts) matches nodes that match
        all the conjuncts.'''
        self.conjuncts = conjuncts

    def is_match(self, g, nodeid):
        return all(c.is_match(g, nodeid) for c in self.conjuncts)

class Not(NodeSpec):

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
        ns = self.see_all(g, nodes)
        if ns:
            return choice(ns) # TODO Weight choice by salience or other
                              # specified criteria?
        else:
            return None

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

class BuildSpec:
    '''Specifies a node to build.'''

    def __init__(self, new_nodeclass, link_specs=None, new_node_args=None):
        self.new_nodeclass = new_nodeclass
        self.link_specs = link_specs
        self.new_node_args = new_node_args

    def already_built(self, g, old_nodeid):
        '''Has a node meeting the spec in relation to old_nodeid already been
        built?'''
        if self.link_specs:
            for link_spec in as_iter(self.link_specs):
                for nodeid in g.neighbors(
                    old_nodeid, port_label=link_spec.old_node_port_label
                ):
                    if self.meets(g, old_nodeid, nodeid):
                        return True
        else:
            # INEFFICIENT: With no link_spec, we examine all nodes in the graph
            return not any(
                g.class_of(nodeid) == self.new_nodeclass
                    for nodeid in g.nodes
            )

    #TODO Check new_node_args
    def meets(self, old_nodeid, nodeid):
        '''Does nodeid meet the spec in relation to old_nodeid?'''
        return (
            g.class_of(nodeid) == self.new_nodeclass
            and
            all(
                meets_link_spec(g, ls, nodeid, old_nodeid)
                    for ls in self.link_specs
            )
        )
