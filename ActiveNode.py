# ActiveNode.py -- Base classes for nodes that can perform Actions

from abc import ABC, abstractmethod

#from PortGraph import Node
from Node import Node
from NodeParams import NodeParams, AttrParam, MateParam
from Action import Action, Actions
from util import as_iter
from exc import Fizzle, NeedArg


class ActiveNode(ABC, Node):
    '''A node that is capable of generating Action objects.

    An ActiveNode always has a .state member, of type ActiveNodeState.'''

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        if self.state is None:
            self.state = Start
        
    #TODO No g param
    @abstractmethod
    def actions(self, g: 'G') -> Actions:
        '''g is the current graph.  Should return a collection of Action
        objects.'''
        pass

    #TODO No g param
    def is_dormant(self, g=None):
        '''Return True to prevent TimeStepper from calling .actions() on this
        node.'''
        return (
            not self.state.is_active(self.g, self)
            or
            self.g.is_failed(self)
        )

    def on_completion(self):
        '''Called when the ActiveNode has completed its business.

        The default implementation cuts all activation and inhibition.'''
        #TODO Notify any "parent" ActionNode.
        #TODO Remove excitation/inhibition edges, not support edges
        #g.remove_support_edges(thisid)
        # TODO Better: zero the weights rather than remove the edges.
        self.g.remove_outgoing_activation_edges(self)
        self.g.remove_incoming_activation_edges(self)


class ActiveNodeState:

    @classmethod
    def is_active(self, g, thisid):
        '''Should TimeStepper even bother to call the ActiveNode's .actions()
        method?'''
        return True

    @classmethod
    def is_completed(self, g, thisid):
        '''Does this state mean that the ActiveNode has finished its
        business?'''
        return False

class Start(ActiveNodeState):
    pass

class Dormant(ActiveNodeState):

    @classmethod
    def is_active(self, g, thisid):
        return False

class Completed(Dormant):

    def is_completed(self, g, thisid):
        return True


class ActionNode(ActiveNode):
    '''A node that holds an action and tries to perform it.'''
    node_params = NodeParams(
        AttrParam('action'),
        AttrParam('state'),
        MateParam('rm_on_success', 'tags')
    )
    initial_activation = 0.1

    def actions(self, g: 'G') -> Actions:
        if not self.is_dormant():
            return [self.action.with_overrides_from(g, self)]
        # TODO If action has any missing args, make scout actions to fill them
        # in.

        # Otherwise return a version of the action with those args filled in.

    def action_failed(self, g, thisid, exc: Fizzle):
        failed_tag = g.add_node('Failed', reason=exc, taggees=[thisid])
        g.add_support(thisid, failed_tag, 1.0)


class ActionSeqNode(ActiveNode):
    '''A group node whose members are a sequence of ActionNodes.'''
    node_params = NodeParams(
        MateParam('members', 'member_of'),
        AttrParam('action_nodes')  # HACK: must be a list, to indicate sequence
    )

    def actions(self, g: 'G') -> Actions:
        return None

    def on_build(self):
        # Give activation to each member and make each member inhibit all
        # following members.
        members = as_iter(self.action_nodes)
        for i, member in enumerate(members):
            self.g.set_activation_from_to(self, member, 0.3)
            self.g.add_edge(self, 'child_action', member, 'parent_action')
            for later_member in members[i+1:]:
                self.g.set_activation_from_to(member, later_member, -1.0)
            for next_member in members[i+1:i+2]:
                self.g.add_edge(member, 'next_action', next_member, 'prev_action')

def make_action_sequence(g, *actions: Action, **kwargs):
    '''Makes an ActionNode to hold each Action, and an ActionSeqNode that
    contains them, in sequence. Returns the nodeid of the ActionSeqNode.'''
    action_nodes = [g.add_node(ActionNode, action=a) for a in actions]
    seqnode = g.add_node(
        ActionSeqNode, action_nodes=action_nodes, members=action_nodes, **kwargs
    )
    return seqnode
