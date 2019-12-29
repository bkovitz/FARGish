# NodeBases.py -- Base classes for fundamental node types

from abc import ABC, abstractmethod

from PortGraph import Node, Tag
from util import nice_object_repr, empty_set


class GroupDescriptor(Tag):
    '''Base class for tags that describe some property of a group, such as a
    CoarseView.'''
    pass


class ActiveNode(ABC, Node):
    '''A node that is capable of generating Action objects.'''

    @abstractmethod
    def actions(self, g, thisid):
        '''g is the current graph, and thisid is the id of the ActiveNode.
        Should return a collection of Action objects.'''
        pass

    def dormant(self, g, thisid):
        '''Return True to prevent TimeStepper from calling .actions() on this
        node.'''
        return False


class Action(ABC):
    '''An action to be performed on the graph.'''

    threshold = 0.0
    # weight must be >= threshold for Action.go() to be called

    weight = 0.1

    on_behalf_of = None
    # The ActiveNode, if any, that produced this action. Descendant classes
    # that implement actions for ActiveNodes should override on_behalf_of
    # in their self.__init__().

    @abstractmethod
    def go(self, g):
        '''Updates g (the host graph) and returns None.'''
        #TODO .go should return some sort of result or disposition, if only
        #to print in log files.
        pass

    __repr__ = nice_object_repr


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
