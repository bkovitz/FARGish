# NodeBases.py -- Base classes for fundamental node types

from abc import ABC, abstractmethod

from PortGraph import Node
from util import nice_object_repr


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
