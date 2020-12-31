# ActiveNode.py -- Base classes for nodes that can perform Actions

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Callable
from random import choice

#from PortGraph import Node
from Node import Node, MaybeNRef
from NodeParams import NodeParams, AttrParam, MateParam
from Action import Action, Actions, BuildAgent, BoostFromTo
from util import as_iter, ClassStrIsName, union, intersection
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
        raise NotImplementedError

    def on_blocked(self) -> Actions:
        '''Actions to perform when this node is Blocked.'''
        agents = self.g.neighbors(self, 'agents')
        agents_problems = union(*(
            self.g.neighbors(agent, 'problem')
                for agent in agents
        ))
        tags_with_agent, tags_without_agent = [], []
        for tag in self.g.tags_of(self, 'Blocked'):
            if tag in agents_problems:
                tags_with_agent.append(tag)
            else:
                tags_without_agent.append(tag)
        if tags_without_agent:
            return BuildAgent(self, choice(tags_without_agent))
        else:
            return BoostFromTo(
                intersection(
                    agents,
                    self.g.neighbors(tags_with_agent, neighbor_label='problem')
                )
            )

    def update(self) -> Actions:
        '''Should return any self-update Actions that this node needs to
        perform, such as SelfDestruct if the node is a Tag that notices that
        it's no longer true. If .update() returns any Actions, then .actions()
        does not get called; updates are actions to run *instead of* the
        node's main actions. By default, returns None.'''
        pass

    def update_asup(self) -> None:
        '''Should make any needed changes to the node's activation and
        support edges.'''
        pass

    # TODO rm; mv body to .can_go().
    def is_dormant(self):
        '''Return True to prevent TimeStepper from calling .actions() on this
        node.'''
        if not self.state:
            return False
        return (
            not self.state.is_active(self.g, self)
            or
            (self.g.is_blocked(self) and not self.on_blocked())
            or
            self.g.is_failed(self)
        )

    def awaken_if_done_sleeping_next_t(self):
        if self.state:
            self.state.awaken_if_done_sleeping_next_t(self.g, self)
            
    def on_completion(self):
        '''Called when the ActiveNode has completed its business.

        The default implementation cuts all activation and inhibition.'''
        #TODO Notify any "parent" ActionNode.
        #TODO Remove excitation/inhibition edges, not support edges
        #g.remove_support_edges(thisid)
        # TODO Better: zero the weights rather than remove the edges.
        self.g.remove_outgoing_activation_edges(self)
        self.g.remove_incoming_activation_edges(self)

class ActiveNodeState(metaclass=ClassStrIsName):

    @classmethod
    def is_active(cls, g, thisid):
        '''Should TimeStepper even bother to call the ActiveNode's .actions()
        method?'''
        return not cls.is_completed(g, thisid)

    @classmethod
    def is_completed(cls, g, thisid):
        '''Does this state mean that the ActiveNode has finished its
        business?'''
        return False

    @classmethod
    def is_sleeping(cls, g, node: MaybeNRef) -> bool:
        return False

    @classmethod
    def awaken_if_done_sleeping_next_t(cls, g: 'G', node: MaybeNRef):
        pass

class Start(ActiveNodeState):
    pass

class Dormant(ActiveNodeState):

    @classmethod
    def is_active(self, g, thisid):
        return False

@dataclass
class Sleeping(Dormant):
    saved_state: ActiveNodeState
    until: Union[int, None]

    def is_sleeping(self, g, node):
        return True

    def awaken_if_done_sleeping_next_t(self, g, node):
        if self.until is None:
            if g.activation(node) >= 1.0:
                g.new_state(node, Start)
        elif g.t + 1 >= self.until:
            g.new_state(node, self.saved_state)

class Completed(Dormant):

    @classmethod
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

    def actions(self):
        if self.g.is_blocked(self):
            return self.on_blocked()
        else:
            return self.action.with_overrides_from(self.g, self)

#    def action_blocked(self, exc: Fizzle):
#        if hasattr(self.action, 'action_blocked'):
#            self.action.action_blocked(self.g, self, exc)
#        else:
#            blocked_tag = self.g.add_node('Blocked', reason=exc, taggees=self)
#            self.g.set_activation_from_to(self, blocked_tag)
#            self.g.add_support(self, blocked_tag, 1.0)
#            self.transient_inhibit_all_next()
#            self.g.reset_activation(self)
#
#    def action_failed(self, exc: ActionFailure):
#        failed_tag = self.g.add_node('Failed', reason=exc, taggees=self)
#        self.g.set_activation_from_to(self, failed_tag)
#        self.g.add_support(self, failed_tag, 1.0)
#        self.transient_inhibit_all_next()
#        self.g.reset_activation(self)
        
    def display_name(self):
        #action_name = self.action.__class__.__name__
        if self.name:
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

    def update_asup(self):
        for member in self.g.members_of(self):
            self.g.set_activation_from_to(
                self, member, 0.5 if self.g.is_active(member) else 0.0
            )

# Move to ActiveGraph
def make_action_sequence(g, *actions: Action, **kwargs):
    # TODO Change type hint so actions can be ActiveNodes
    '''Makes an ActionNode to hold each Action, and an ActionSeqNode that
    contains them, in sequence. Returns the nodeid of the ActionSeqNode.'''
    #action_nodes = [g.add_node(ActionNode, action=a) for a in actions]
    action_nodes = [g.add_node(a) for a in actions]
    g.link_sequence(action_nodes)
    seqnode = g.add_node(
        #ActionSeqNode, action_nodes=action_nodes, members=action_nodes, **kwargs
        ActionSeqNode, members=action_nodes, **kwargs
    )
    return seqnode

class HasUpdate(ActiveNode):
    '''Mix-in for a Node that needs to have its .update() function called
    when it's touched.'''
    update_action: Actions = None

    def on_touch(self):
        self.needs_update = True
        super().on_touch()

    def update(self):
        return self.update_action
