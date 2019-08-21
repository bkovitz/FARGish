# PortGraph.py -- PortGraph class

import networkx as nx

from collections import defaultdict, namedtuple, UserDict

empty_set = frozenset()

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

    def hops_from_port_label(self, from_port_label):
        '''Returns a set of Hops.'''
        try:
            return self.d_from_port_label[from_port_label]
        except KeyError:
            return empty_set

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


class PortGraph(nx.MultiGraph):

    node_dict_factory = NodeAttrDict

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.nextid = 1

    def _bump_nextid(self):
        result = self.nextid
        self.nextid += 1
        return result

    def make_node(self, attrs):
        i = self._bump_nextid()
        self.add_node(i, **attrs)
        return i

    def dup_node(self, h, node):
        'h is another PortGraph, which must contain node.'
        attrs = h.nodes[node]
        new_attrs = dict((k, attrs[k]) for k in ['_class', 'value']
                                           if k in attrs)
        return self.make_node(new_attrs)

    def nodestr(self, node):
        attrs = self.nodes[node]
        cl = attrs.get('_class', None)
        value = attrs.get('value', None)
        if cl is not None:
            class_name = cl.__name__
            if value is not None:
                return '%s: %s(%s)' % (node, class_name, value)
            else:
                return '%s: %s' % (node, class_name)
        else:
            return str(node)

    def add_edge(self, node1, port_label1, node2, port_label2, **attr):
        key = super(PortGraph, self).add_edge(node1, node2, **attr)
        hop1 = Hop(node1, port_label1, node2, port_label2, key)
        hop2 = Hop(node2, port_label2, node1, port_label1, key)
        self.nodes[node1]['_hops'].add(hop1)
        self.nodes[node2]['_hops'].add(hop2)
        return key

    def remove_edge(self, node1, port_label1, node2, port_label2):
        hop = self.find_hop(node1, port_label1, node2, port_label2)
        if hop:
            self.nodes[node1]['_hops'].remove(hop)
            self.nodes[node2]['_hops'].remove(hop.reverse())
            super().remove_edge(node1, node2, hop.key)

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

    def neighbors(self, node, port_label=None):
        if port_label is None:
            return super().neighbors(node)
        else:
            return (hop.to_node
                        for hop in self.hops_from_port(node, port_label))

    def add_member_edge(self, group_node, member_node):
        self.add_edge(group_node, 'members', member_node, 'member_of')

    def members_of(self, group_node):
        return list(self.neighbors(group_node, 'members'))

    def members_to_subgraph(self, group_node):
        return self.subgraph(self.members_of(group_node))


class Node:

    @classmethod
    def is_attrs_match(cls, node_attrs, host_node_attrs):
        return True

    @classmethod
    def d(cls, value=None):
        if value is None:
            return {'_class': cls}
        else:
            return {'_class': cls, 'value': value}


class Number(Node):

    @classmethod
    def is_attrs_match(cls, node_attrs, host_node_attrs):
        try:
            return (
                node_attrs['value'] == host_node_attrs['value']
            )
        except KeyError:
            return False


def is_node_match(tg, target_node, hg, host_node):
    try:
        target_attrs = tg.nodes[target_node]
        host_attrs = hg.nodes[host_node]
        target_class = target_attrs['_class']
        host_class = host_attrs['_class']
        return (
            issubclass(host_class, target_class)
            and
            target_class.is_attrs_match(target_attrs, host_attrs)
        )
    except KeyError:
        return False

def is_port_label_match(target_port_label, host_port_label):
    if (
        isinstance(target_port_label, str)
        or
        isinstance(target_port_label, numbers.Number)
    ):
        return target_port_label == host_port_label
    else:
        return issubclass(host_port_label, target_port_label)


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
