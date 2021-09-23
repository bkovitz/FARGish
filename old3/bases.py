# NodeBases.py -- Base classes for fundamental node types

from abc import ABC, abstractmethod
from collections import namedtuple

from PortGraph import Node, Tag
from util import nice_object_repr, empty_set


# Ports to link by an edge between an old node and a new node
NewLinkSpec = namedtuple('NewLinkSpec',
    ['old_node_port_label', 'new_node_port_label']
)
def make_link(g, link_spec, new_node, old_node):
    g.add_edge(old_node, link_spec.old_node_port_label,
               new_node, link_spec.new_node_port_label)

def meets_link_spec(g, link_spec, new_node, old_node):
    return g.has_hop(
        new_node,
        link_spec.new_node_port_label,
        old_node,
        link_spec.old_node_port_label
    )


class GroupDescriptor(Tag):
    '''Base class for tags that describe some property of a group, such as a
    CoarseView.'''
    pass


class NodeIsA:
    '''A function that returns true iff a given node is an instance of
    nodeclass.'''

    def __init__(self, nodeclass):
        self.nodeclass = nodeclass

    def __call__(self, g, nodeid):
        return g.is_of_class(self, nodeid, self.nodeclass)

    __repr__ = nice_object_repr


class AllNodesOfClass:
    '''A function that returns a list of all nodes of class nodeclass.'''

    def __init__(self, nodeclass):
        self.nodeclass = nodeclass

    def __call__(self, g, nodeid):
        return g.nodes_of_class(self.nodeclass)

    __repr__ = nice_object_repr


class AllNodesTagged:
    '''A function that returns a list of all nodes tagged by tagclass via
    taggee_port_label.'''

    def __init__(self, tagclass, taggee_port_label='tags'):
        self.tagclass = tagclass
        self.taggee_port_label = taggee_port_label

    def __call__(self, g, nodeid):
        return g.nodes_with_tag(
            self.tagclass,
            taggee_port_label=self.taggee_port_label
        )

    __repr__ = nice_object_repr


class CoarseView(Node):

    def __init__(self, find_nodes):
        '''find_nodes is function(g, nodeid) that returns set of node to
        include in the CoarseView. An AllNodesOfClass object is such a function
        (q.v.).'''
        self.find_nodes = find_nodes
        self.nodes = empty_set

    def update(self, g, nodeid):
        new_nodes = set(self.find_nodes(g, nodeid))
        if new_nodes != self.nodes:
            self.nodes = new_nodes
            g.reset_hops_from_port(nodeid, 'viewees', new_nodes, 'viewers')
            g.remove_tag(nodeid, GroupDescriptor)
