# ActiveNode.py -- Base classes for nodes that can perform Actions

from abc import ABC, abstractmethod

from PortGraph import Node
from NodeParams import NodeParams, AttrParam, MateParam


class ActiveNode(ABC, Node):
    '''A node that is capable of generating Action objects.

    An ActiveNode always has a .state member, of type ActiveNodeState.'''

    def __init__(self, *args, **kwargs):
        super().__init__(*args, *kwargs)
        if self.state is None:
            self.state = Start
        
    @abstractmethod
    def actions(self, g, thisid):
        '''g is the current graph, and thisid is the id of the ActiveNode.
        Should return a collection of Action objects.'''
        pass

    # TODO replace with is_dormant
    def dormant(self, g, thisid):
        '''Return True to prevent TimeStepper from calling .actions() on this
        node.'''
        return not self.state.is_active(g, thisid)


class ActiveNodeState:

    @classmethod
    def is_active(self, g, thisid):
        '''Should TimeStepper even bother to call the ActiveNode's .actions()
        method?'''
        return True

class Start(ActiveNodeState):
    pass

class Dormant(ActiveNodeState):

    @classmethod
    def is_active(self, g, thisid):
        return False

class Completed(Dormant):
    pass


class ActionNode(ActiveNode):
    node_params = NodeParams(AttrParam('action'), AttrParam('state'))
    #NEXT Start the ActionNode off with a state of 'NotDone'.

    def actions(self, g, thisid):
        return [self.action]
        # TODO If action has any missing args, make scout actions to fill them in.

        # Otherwise return a version of the action with those args filled in.
        pass
