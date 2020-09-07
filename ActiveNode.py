# ActiveNode.py -- Base classes for nodes that can perform Actions

from abc import ABC, abstractmethod

from PortGraph import Node
from NodeParams import NodeParams, AttrParam, MateParam
from util import as_iter


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
    '''A node that performs an action.'''
    node_params = NodeParams(AttrParam('action'), AttrParam('state'))

    def actions(self, g, thisid):
        return [self.action]
        # TODO If action has any missing args, make scout actions to fill them in.

        # Otherwise return a version of the action with those args filled in.


class ActionSeqNode(Node):
    '''A group node whose members are a sequence of ActionNodes.'''
    node_params = NodeParams(
        MateParam('members', 'member_of'),
        AttrParam('action_nodes')  # HACK: must be a list, to indicate sequence
    )

    def on_build(self, g, thisid):
        # Give activation to each member and make each member inhibit all
        # following members.
        members = as_iter(self.action_nodes)
        for i, member in enumerate(members):
            g.add_support(thisid, member, 0.5)
            for later_member in members[i+1:]:
                #TODO This should be done with a quantity other than support.
                #Maybe add an 'activation' quantity to every ActiveNode.
                g.oppose(member, later_member)
