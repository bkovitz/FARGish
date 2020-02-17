# LinkSpec.py -- The LinkSpec class

from inspect import isclass
from copy import copy

from util import as_iter, is_nodeid, NiceRepr


class LinkSpec:
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
        self.old_node = old_node
        self.old_node_matcher = make_node_matcher(old_node)

    def __repr__(self):
        if self.old_node is None:
            return f'LinkSpec({repr(self.old_node_port_label)}, {repr(self.new_node_port_label)})'
        else:
            return f'LinkSpec({repr(self.old_node_port_label)}, {repr(self.new_node_port_label)}, {repr(self.old_node)})'
    
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
