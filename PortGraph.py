# PortGraph.py -- PortGraph class

from util import nice_object_repr, as_iter

import networkx as nx

from collections import defaultdict, namedtuple, UserDict
from inspect import isclass
from operator import attrgetter


empty_set = frozenset()


class Node:

    def is_attrs_match(self, other):
        return True

    __repr__ = nice_object_repr


class Tag(Node):
    pass

class CouldMake(Tag):

    def __init__(self, bindings):
        self.bindings = bindings


HopBase = namedtuple('Hop',
    ['from_node', 'from_port_label', 'to_node', 'to_port_label', 'key']
)
class Hop(HopBase):
    def reverse(self):
        return Hop(
            self.to_node,
            self.to_port_label,
            self.from_node,
            self.from_port_label,
            self.key
        )

class HopDict:

    def __init__(self):
        self.d_from_port_label = {}  # from_port_label: set(Hop)
        self.d_to_node = {}          # to_node: set(Hop)

    def all_hops(self):
        '''Returns a set of hops.'''
        result = set()
        for hset in self.d_from_port_label.values():
            result = result.union(hset)
        return result

    def from_port_labels(self):
        return self.d_from_port_label.keys()

    def add(self, hop):
        try:
            self.d_from_port_label[hop.from_port_label].add(hop)
        except KeyError:
            self.d_from_port_label[hop.from_port_label] = set([hop])
        try:
            self.d_to_node[hop.to_node].add(hop)
        except KeyError:
            self.d_to_node[hop.to_node] = set([hop])

    def remove(self, hop):
        '''It is not an error to remove a hop that doesn't exist.'''
        try:
            self.d_from_port_label[hop.from_port_label].discard(hop)
        except KeyError:
            pass
        try:
            self.d_to_node[hop.to_node].discard(hop)
        except KeyError:
            pass

    def remove_all_hops_to(self, to_node):
        '''It is not an error if there are no hops to to_node.'''
        for hop in list(self.hops_to_neighbor(to_node)):
            self.remove(hop)

    def hops_from_port_label(self, from_port_label):
        '''Returns a set of Hops.'''
        try:
            return self.d_from_port_label[from_port_label]
        except KeyError:
            return empty_set

    def hops_to_port_label(self, to_port_label):
        '''Returns a seq of hops.'''
        return (hop for hop in self.all_hops()
                       if hop.to_port_label == to_port_label)

    def hops_to_neighbor(self, neighbor_node):
        '''Returns a set of Hops.'''
        try:
            return self.d_to_node[neighbor_node]
        except KeyError:
            return empty_set


class NodeAttrDict(UserDict):
    '''Custom dict that maps nodes in a Graph to their attributes. Every
    node automatically gets an attribute named '_hops' whose value is a
    defaultdict(set).'''

    def __setitem__(self, node, node_attrs):
        '''node_attrs must be a dictionary object.'''
        super().__setitem__(node, node_attrs)
        self.data[node]['_hops'] = HopDict()


# Convenience class for holding a node's id along with the value stored in 
# its datum.
NodeAndValue = namedtuple('NodeAndValue', ['node', 'value'])


class PortGraph(nx.MultiGraph):

    node_dict_factory = NodeAttrDict

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.nextid = 1

    def _bump_nextid(self):
        result = self.nextid
        self.nextid += 1
        return result

    def make_node(self, o):
        i = self._bump_nextid()
        if isclass(o):
            o = o()
        self.add_node(i, **{'datum': o})
        return i

    def dup_node(self, h, node):
        'h is another PortGraph, which must contain node.'
        attrs = h.nodes[node]
        new_attrs = dict((k, attrs[k]) for k in ['_class', 'value']
                                           if k in attrs)
        return self.make_node(new_attrs)

#    def nodestr(self, node):
#        attrs = self.nodes[node]
#        cl = attrs.get('_class', None)
#        value = attrs.get('value', None)
#        if cl is not None:
#            class_name = cl.__name__
#            if value is not None:
#                return '%s: %s(%s)' % (node, class_name, value)
#            else:
#                return '%s: %s' % (node, class_name)
#        else:
#            return str(node)

    def nodestr(self, node):
        return str(node) + ': ' + self.datumstr(node)

    def datumstr(self, node):
        return str(self.datum(node))

    def add_edge(self, node1, port_label1, node2, port_label2, **attr):
        '''If the edge already exists, doesn't make a new one. Regardless,
        returns the key of the edge.'''
        hop = self.find_hop(node1, port_label1, node2, port_label2)
        if hop:
            return hop.key
        else:
            key = super(PortGraph, self).add_edge(node1, node2, **attr)
            hop1 = Hop(node1, port_label1, node2, port_label2, key)
            hop2 = Hop(node2, port_label2, node1, port_label1, key)
            self.nodes[node1]['_hops'].add(hop1)
            self.nodes[node2]['_hops'].add(hop2)
            return key

    def has_edge(self, u, v, w=None, y=None):
        if y is None:
            return super().has_edge(u, v, w)
        else:
            node1 = u
            node1_port_label = v
            node2 = w
            node2_port_label = y
            return (
                self.has_hop(node1, node1_port_label, node2, node2_port_label)
                or  # Is the 'or' necessary? Is overriding has_edge() necessary,
                    # since we have has_hop()?
                self.has_hop(node2, node2_port_label, node1, node1_port_label)
            )

    def remove_edge(self, node1, port_label1, node2, port_label2):
        hop = self.find_hop(node1, port_label1, node2, port_label2)
        if hop:
            self.nodes[node1]['_hops'].remove(hop)
            self.nodes[node2]['_hops'].remove(hop.reverse())
            super().remove_edge(node1, node2, hop.key)

    def remove_node(self, node):
        self._remove_all_hops_to(node)
        super().remove_node(node)

    def remove_nodes_from(self, nodes):
        nodes = list(nodes) # We iterate over nodes twice.
        for node in nodes:
            self._remove_all_hops_to(node)
        super().remove_nodes_from(nodes)

    def _remove_all_hops_to(self, node):
        for neighbor in list(self.neighbors(node)):
            self.hopdict(neighbor).remove_all_hops_to(node)

    def hops_from_node(self, node):
        return self.nodes[node]['_hops'].all_hops()

    def hops_from_port(self, node, port_label):
        return self.nodes[node]['_hops'].hops_from_port_label(port_label)

    def hops_to_neighbor(self, node, neighbor_node):
        return (
            self.nodes[node]['_hops'].hops_to_neighbor(neighbor_node)
        )

    def port_labels(self, node):
        return self.nodes[node]['_hops'].from_port_labels()

    def find_hop(self, from_node, from_port_label, to_node, to_port_label):
        '''Returns the Hop if it exists, else None.'''
        try:
            return next(hop for hop in self.hops_to_neighbor(from_node, to_node)
                                if hop.from_port_label == from_port_label
                                    and
                                    hop.to_port_label == to_port_label)
        except StopIteration:
            return None

    def edge_to_hop(self, edge):
        'edge is a tuple (from_node, to_node, key). Returns a Hop or None.'
        from_node, to_node, key = edge
        try:
            return next(hop for hop in self.hops_to_neighbor(from_node, to_node)
                            if hop.key == key)
        except StopIteration:
            return None

    def has_hop(self, from_node, from_port_label, to_node, to_port_label):
        return bool(
            self.find_hop(from_node, from_port_label, to_node, to_port_label)
        )

    def add_tag(
        self, tag_or_tagclass, node_or_nodes,
        tag_port_label='taggees', node_port_label='tags'
    ):
        '''Links a tag to one or more nodes. Returns the tag's id.
        If tag_or_tagclass is a class, builds the tag.'''
        if isclass(tag_or_tagclass):
            # Make the tag node
            tag = self.make_node(tag_or_tagclass)
        else:
            # We'll link from an existing tag node
            tag = tag_or_tagclass
        # Link the tag to the taggee(s)
        for node in as_iter(node_or_nodes):
            self.add_edge(tag, tag_port_label, node, node_port_label)
        return tag

    def replace_tag(
        self, node_or_nodes, old_tag_or_tagclass, new_tag_or_tagclass,
        tag_port_label='taggees', node_port_label='tags'
    ):
        '''Removes old_tag_or_tagclass, adds new_tag_or_tagclass to
        node_or_nodes.'''
        self.remove_tag(node_or_nodes, old_tag_or_tagclass)
        self.add_tag(tag_or_tagclass=new_tag_or_tagclass,
                     node_or_nodes=node_or_nodes,
                     tag_port_label=tag_port_label,
                     node_port_label=node_port_label)

    def has_tag(self, node, tagclass):
        return any(
            tag for tag in self.neighbors(node, port_label='tags')
                    if issubclass(self.class_of(tag), tagclass)
        )

    def tags_of(self, node, tagclass=Tag):
        'Returns a generator.'
        #TODO Should be able to specify tagclass more narrowly.
        return (tag for tag in self.neighbors(node, port_label='tags')
                        if issubclass(self.class_of(tag), tagclass))

    #TODO node_or_nodes, tag_or_tagclass
    def remove_tag(self, node_or_nodes, tag_or_tagclass):
        '''Removes all tags of node that match tagclass.'''
        #TODO Should only remove the edge if the tag tags other nodes, too.
        #TODO Should be able to specify the tagclass more narrowly (e.g.
        # by value, by other taggees).
        if isclass(tag_or_tagclass):
            for node in as_iter(node_or_nodes):
                self.remove_nodes_from(
                    tag for tag in self.tags_of(node, tagclass=tag_or_tagclass)
                )
        else:
            self.remove_node(tag_or_tagclass)
        
    def neighbors(self, node, port_label=None):
        if port_label is None:
            return super().neighbors(node)
        else:
            return set(hop.to_node
                          for hop in self.hops_from_port(node, port_label))

    def neighbor(self, node, port_label=None):
        '''Returns 'first' neighbor of node, at optional port_label. If there
        is more than one neighbor, the choice is made arbitrarily. If there
        is no neighbor, returns None.'''
        try:
            return next(iter(self.neighbors(node, port_label)))
        except StopIteration:
            return None

    def add_member_edge(self, group_node, member_node):
        self.add_edge(group_node, 'members', member_node, 'member_of')

    def members_of(self, group_node):
        return list(self.neighbors(group_node, 'members'))

    def members_to_subgraph(self, group_node):
        return self.subgraph(self.members_of(group_node))

    def is_member(self, group_node, node):
        return self.has_hop(group_node, 'members', node, 'member_of')

    def hopdict(self, node):
        return self.nodes[node]['_hops']

    def class_of(self, node):
        '''Returns None is node does not exist or lacks a datum.'''
        datum = self.datum(node)
        if datum is None:
            return None
        else:
            return self.datum(node).__class__

    def is_of_class(self, node, cl):
        try:
            return issubclass(self.class_of(node), cl)
        except TypeError:
            return False

    def value_of(self, node):
        try:
            return self.datum(node).value
        except AttributeError:
            return None

    def have_same_value(self, node1, node2):
        '''A value of None is not considered the same as anything, even another
        None.'''
        v1 = self.value_of(node1)
        if v1 is None:
            return False
        v2 = self.value_of(node2)
        if v2 is None:
            return False
        return v1 == v2

    def datum(self, node):
        '''Returns the datum associated with node. The datum is presumed to be
        an object that inherits from Node. If node does not exist or lacks
        a datum, returns None.'''
        try:
            return self.nodes[node]['datum']
        except KeyError:
            return None

    def find_member_in_role(self, group_node, role):
        #TODO UT
        '''role is a port label that a neighbor of the sought node must have.
        Returns either a node playing that role in relation to another member
        of group_node, or None. If more than one node fits the criterion, the
        choice is arbitrary.'''
        for member in self.members_of(group_node):
            for hop in self.hopdict(member).hops_to_port_label(role):
                if self.is_member(group_node, hop.to_node):
                    return member
        return None

    def nodes_of_class(self, cl):
        #TODO UT
        result = []
        for node in self.nodes:
            datum = self.nodes[node]['datum']
            if issubclass(datum.__class__, cl):
                result.append(node)
        return result

    def nodes_with_tag(self, tagclass, nodes=None):
        'Returns a generator of nodes that have a tag of class tagclass.'
        if nodes is None:
            nodes = self.nodes
        return (
            node for node in nodes
                     if self.has_tag(node, tagclass)
        )

    def nodes_without_tag(self, tagclass, nodes=None):
        'Returns a generator of nodes that do not have a tag of class tagclass.'
        if nodes is None:
            nodes = self.nodes
        return (
            node for node in nodes
                     if not self.has_tag(node, tagclass)
        )

    def is_in_role(self, node, role):
        'role is the port label of a neighbor of node.'
        return any(self.hopdict(node).hops_to_port_label(role))


#    @classmethod
#    def is_attrs_match(cls, node_attrs, host_node_attrs):
#        try:
#            return (
#                node_attrs['value'] == host_node_attrs['value']
#            )
#        except KeyError:
#            return False


#def is_node_match(tg, target_node, hg, host_node):
#    try:
#        target_attrs = tg.nodes[target_node]
#        host_attrs = hg.nodes[host_node]
#        #target_class = target_attrs['_class']
#        #host_class = host_attrs['_class']
#        target_class = target_attrs['datum'].__class__
#        host_class = host_attrs['datum'].__class__
#        return (
#            issubclass(host_class, target_class)
#            and
#            target_class.is_attrs_match(target_attrs, host_attrs)
#        )
#    except KeyError:
#        return False

def is_node_match(bg, base_node, hg, host_node):
    try:
        base_datum = bg.nodes[base_node]['datum']
        host_datum = hg.nodes[host_node]['datum']
    except KeyError:
        return False
    return (
        issubclass(host_datum.__class__, base_datum.__class__)
        and
        base_datum.is_attrs_match(host_datum)
    )

def is_port_label_match(target_port_label, host_port_label):
    if (
        isinstance(target_port_label, str)
        or
        isinstance(target_port_label, numbers.Number)
    ):
        return target_port_label == host_port_label
    else:
        return issubclass(host_port_label, target_port_label)

def pt(g):
    if 't' in g.graph:
        print('t=' + str(g.graph['t']))

def pn(g):
    '''Prints the nodes in the graph'''
    pt(g)
    for node in g.nodes:
        print(g.nodestr(node))

def pg(g):
    '''Prints graph g in simple text form.'''
    pt(g)
    for node in g.nodes:
        print(g.nodestr(node))
        for hop in sorted(
            g.hops_from_node(node), key=attrgetter('from_port_label')
        ):
            print('  %s --> %s %s' % (hop.from_port_label,
                                      g.nodestr(hop.to_node),
                                      hop.to_port_label))

if __name__ == '__main__':
    g = PortGraph()
    g.add_edge('A', 'in', 'B', 'out')
    g.add_edge('O', 'members', 'A', 'member_of')
    #print(g.nodes(data=True))
    print(list(g.neighbors('A')))
    print(list(g.neighbors('A', 'in')))
    print(list(g.neighbors('B', 'out')))
    print(g.hops_to_neighbor('A', 'B'))
#    g.remove_edge('A', 'in', 'B', 'out')
#    print(list(g.neighbors('A')))
#    print(list(g.neighbors('A', 'in')))
#    print(list(g.neighbors('B', 'out')))

    #print(g.adj)
    #print(list(g.adjacency()))
    #g.remove_edge('A', 'in', 'B', 'out')
