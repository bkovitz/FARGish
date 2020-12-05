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
            self.state = Start   # HACK
        
    def can_go(self) -> bool:
        '''Can this ActiveNode generate any Actions this timestep? Should
        return False if the node is in a dormant state, Blocked, or otherwise
        should not even be polled by ActiveGraph.do_timestep() to see what
        Actions it wants to do right now.'''
        return not self.is_dormant()

    @abstractmethod
    def actions(self) -> Actions:
        '''What Actions does this ActiveNode want to do in this timestep?
        We assume that .can_go() has been called and has returned True before
        .actions() is called. So, it is not necessary to check .state.'''
        pass

    # TODO rm; mv body to .can_go().
    def is_dormant(self):
        '''Return True to prevent TimeStepper from calling .actions() on this
        node.'''
        return (
            not self.state.is_active(self.g, self)
            or
            self.g.is_blocked(self)
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
        AttrParam('state', Start),
        MateParam('rm_on_success', 'tags')
    )
    initial_activation = 0.1

    #TODO rm
    def OLDactions(self, g: 'G') -> Actions:
        if not self.is_dormant():
            return [self.action.with_overrides_from(g, self)]
        # TODO If action has any missing args, make scout actions to fill them
        # in.

        # Otherwise return a version of the action with those args filled in.

    def actions(self):
        return self.action.with_overrides_from(self.g, self)

    def action_blocked(self, exc: Fizzle):
        if hasattr(self.action, 'action_blocked'):
            self.action.action_blocked(self.g, self, exc)
        else:
            failed_tag = self.g.add_node('Blocked', reason=exc, taggees=self)
            self.g.set_activation_from_to(self, failed_tag)
            self.g.add_support(self, failed_tag, 1.0)
            self.transient_inhibit_all_next()
            self.g.reset_activation(self)

    def display_name(self):
        #action_name = self.action.__class__.__name__
        if hasattr(self, 'name'):
            return self.name
        elif not self.action:
            return super().display_name()
        else:
            # TODO Put an * after the action's class name
            return str(self.action)

class ActionSeqNode(ActiveNode):
    '''A group node whose members are a sequence of ActionNodes.'''
    node_params = NodeParams(
        MateParam('members', 'member_of'),
    )

    def actions(self) -> Actions:
        return None

    def on_build(self):
        # Give activation to each member and make each member inhibit all
        # following members.
        #print('ONB', self.id, self.g.members_of(self))
        for member in self.g.members_of(self):
            self.g.set_activation_from_to(self, member, 0.5)
            self.g.inhibit_all_next(member)

def make_action_sequence(g, *actions: Action, **kwargs):
    '''Makes an ActionNode to hold each Action, and an ActionSeqNode that
    contains them, in sequence. Returns the nodeid of the ActionSeqNode.'''
    action_nodes = [g.add_node(ActionNode, action=a) for a in actions]
    g.link_sequence(action_nodes)
    seqnode = g.add_node(
        #ActionSeqNode, action_nodes=action_nodes, members=action_nodes, **kwargs
        ActionSeqNode, members=action_nodes, **kwargs
    )
    return seqnode
