# TimeStepper.py -- Mix-in class to run a timestep in a FARG model
#
# The main method, .do_timestep(), chooses nodes for activity, queries them
# for Actions, chooses Actions, and runs them.

from operator import attrgetter
from random import choice
from typing import Union, List, Tuple
import inspect

from PortGraph import NodesWithSalience, pg
from bases import CoarseView
from Action import Action
from ActiveNode import ActiveNode
from util import sample_without_replacement, empty_set
from exc import FargDone, NeedArg
import support
from log import ShowActiveNodes, ShowActionList, ShowActionsChosen, \
    ShowResults, ShowAnnotations, ShowActionsPerformed
from util import as_iter
from WithActivation import WithActivation, log_activation


class TimeStepper(WithActivation):
    '''This class must be mixed in with a PortGraph.'''

    max_active_nodes = None  # maximum number of ActiveNodes to consider in
                             # in one timestep, or None to consider all
    max_actions = 1  # maximum number of Actions to perform in one timestep

    def __init__(self, *args, **kwargs):
        self.consecutive_timesteps_with_no_action = 0
        kws = kwargs.copy()
        if 'max_active_nodes' in kws:
            self.max_active_nodes = kws['max_active_nodes']
        if 'max_actions' in kws:
            self.max_actions = kws['max_actions']
        if 't' not in kws:
            kws['t'] = 0
        if 'done' not in kws:
            kws['done'] = False
        if 'support_steps' not in kws:
            kws['support_steps'] = 5  # number of support steps per timestep
        if 'activation_steps' not in kws:
            kws['activation_steps'] = 5  # number of activation steps per timestep
        super().__init__(*args, **kws)

    def do_timestep(
        self,
        num=1,
        action: Union[Action, List[Action], None]=None,
        actor: Union[int, None]=None
    ) -> None:
        '''Executes n timesteps.

        On each timestep, we decay saliences, choose active nodes, generate
        Action objects from the active nodes, choose which Actions to
        perform, execute those Actions, and update support.

        If action is non-None, we run the specified Action unconditionally.

        Otherwise, if actor is non-None, we run all the Actions provided by the
        actor (assumed to be an ActiveNode) unconditionally.'''
        for i in range(num):
            self.graph['t'] += 1
            self.clear_touched_and_new()

            self.decay_saliences()  # TODO rm
            self.decay_activations()

            self.propagate_support()
            support.log_support(self)

            self.propagate_activation()
            log_activation(self)

            self.update_coarse_views()

            if action is not None:
                actions_to_do = as_iter(action)
            elif actor is not None:
                actions_to_do = self.collect_actions([actor])
            else:
                actions_to_do = self.get_actions_from_graph()

            if ShowActionsPerformed.is_logging():
                print('ACTIONS PERFORMED')
                if not actions_to_do:
                    print('  (none)')
            for a in actions_to_do:
                if ShowActionsPerformed.is_logging():
                    print(f'  {a.actor}: {a}')
                self.do_action(a)

            self.do_touches()
            self.update_all_support()

            d = self.done()
            if d:
                ShowResults(d)
                ShowResults(f"t={self.graph['t']}\n")
                break

    def do_action_sequence(self, actions: Union[List[Action], Action, None]):
        '''Force a sequence of actions, one per timestep. If a single Action
        is an iterable, then all the Actions it contains will be performed
        on its timestep.'''
        for a in as_iter(actions):
            self.do_timestep(action=a)

    def do_action(self, action: Union[Action, None]):
        '''action: an Action object'''
        if action is None:
            return

        try:
            self.builder = action.actor
        except AttributeError:
            self.builder = None

        try:
            action.go(self)
        except FargDone as exc:
            self.set_done(exc)
        except NeedArg as exc:
            #print('NEEDARG')
            self.call_method(exc.actor, 'action_failed', exc)
        except:
            print('EXCEPTION in do_action')
            print(f'ACTOR: {self.nodestr(action.actor)}  ON BEHALF OF: {self.nodestr(action.on_behalf_of)}')
            print(f'ACTION: {action}')
            raise

        self.builder = None
        if ShowAnnotations.is_logging():
            a = action.annotation()
            if a is not None:
                print(a)

    do = do_action

    def set_done(self, done):
        self.graph['done'] = done

    def done(self):
        try:
            return self.graph['done']
        except KeyError:
            return False

    def succeeded(self):
        d = self.done()
        try:
            return d.succeeded
        except AttributeError:
            return False

    def action_sorting_key(self, action: Action) -> Tuple:
        return (
            self.urgency(action),
            self.activation(action.actor),
            self.support_for(action.actor)
        )

    def print_actions(self, actions: List[Action]):
        if not len(actions):
            print('  (none)')
            return
        headingfmt = '  %5s %5s %7s %5s %7s %4s %s'
        fmt =        '  %.3f %.3f (%.3f) %.3f (%.3f) %4d %s'
        headings = ('u', 'a', '(a-t)', 's', '(s-t)', 'node', 'action')
        print(headingfmt % headings)
        for action in sorted(actions, key=self.action_sorting_key):
            print(fmt % (self.urgency(action),
                         self.activation(action.actor),
                         action.threshold,
                         self.support_for(action.actor),
                         action.support_threshold,
                         action.actor,
                         action))


    def get_actions_from_graph(self):
        '''Polls all the ActiveNodes for Actions and chooses which to
        do on this timestep.'''
        active_nodes = self.get_active_nodes()

        if (self.max_active_nodes is not None
            and
            len(active_nodes) > self.max_active_nodes
        ):
            active_nodes = self.choose_active_nodes(active_nodes)
        if (
            ShowActiveNodes.is_logging()
            or
            ShowActionList.is_logging()
            or
            ShowActionsChosen.is_logging()
        ):
            print(f'{chr(10)}t={self.graph["t"]}')

        if ShowActiveNodes.is_logging():
            print('ACTIVE NODES')
            pg(self, active_nodes)

        actions = self.collect_actions(active_nodes)
        if ShowActionList.is_logging():
            print('ACTIONS COLLECTED')
            self.print_actions(actions)

        # Filter out actions whose weight is below their threshold
        #actions = [a for a in actions if a.weight(self) >= a.threshold]
        actions = [a for a in actions if self.urgency(a) > 0.0]

        if len(actions) == 0:
            self.consecutive_timesteps_with_no_action += 1
            #HACK Crude boosting of random nodes to shake things up
            if self.consecutive_timesteps_with_no_action > 10:
                ns = list(self.nodes)
                for i in range(10):
                    nodeid = choice(ns)
                    #print('GROSS', self.nodestr(nodeid))
                    self.gross_boost_salience(nodeid)
        else:
            self.consecutive_timesteps_with_no_action = 0

        chosen_actions = self.choose_actions(actions)
        if ShowActionsChosen.is_logging():
            print('ACTIONS CHOSEN')
            self.print_actions(chosen_actions)
        return chosen_actions

    def allowable_active_nodes(self):
        '''Returns all nodes in the 'ws' (the workspace), if a 'ws' has been
        set. Otherwise returns all nodes.'''
        try:
            ws = self.graph['ws']
        except KeyError:
            return self.nodes
        try:
            slipnet = {self.graph['slipnet']}  # TODO OAOO
        except KeyError:
            slipnet = empty_set
        return self.members_recursive(ws) | slipnet #HACK

    def get_active_nodes(self):
        '''Must return a collection of nodes.'''
        return list(
            node for node in self.nodes_of_class(
                    ActiveNode,
                    nodes=self.allowable_active_nodes()
                )
                    if not self.is_dormant(node)
        )

    def choose_active_nodes(self, active_nodes, k=None):
        '''Randomly chooses up to k Actions, weighted by activation.
        Returns a collection. k defaults to self.max_active_nodes.'''
        if k is None:
            k = self.max_active_nodes
        return list(sample_without_replacement(
            active_nodes,
            k=k,
            #weights=[self.support_for(node) for node in active_nodes]
            weights=[self.activation(node) for node in active_nodes]
        ))

    def collect_actions(self, active_nodes):
        '''Calls .actions() on each node in active_nodes, and returns all the
        returned objects (presumed to be Actions) in a list.'''
        actions = []
        for node in as_iter(active_nodes):
            #print('COLL', node, self.datum(node))
            got = self.datum(node).actions(self, node)
            for action in as_iter(got):
                if action is not None:
                    action.actor = node
                    actions.append(action)
        return actions

    def choose_actions(self, actions, k=None):
        '''Randomly chooses up to k Actions, weighted by .urgency.
        Returns a collection. k defaults to self.max_actions.'''
        if k is None:
            k = self.max_actions
        return list(sample_without_replacement(
            actions,
            k=k,
            weights=[self.urgency(a) for a in actions]
        ))

    def update_coarse_views(self):
        for nodeid in self.nodes_of_class(CoarseView):
            self.datum(nodeid).update(self, nodeid)

    def urgency(self, action: Action) -> float:
        support = self.support_for(action.actor)
        if support < action.support_threshold:
            return action.min_urgency
        activation = self.activation(action.actor)
        if activation < action.threshold:
            return action.min_urgency
        return max(
            activation - action.threshold,
            action.min_urgency
        )

    def propagate_support(self):
        try:
            propagator = self.graph['support_propagator']
        except KeyError:
            return
        for i in range(self.graph['support_steps']):
            #TODO Why not just put .propagate in self?
            propagator.propagate(self)

    def propagate_activation(self):
        try:
            propagator = self.graph['activation_propagator']
        except KeyError:
            return
        for i in range(self.graph['activation_steps']):
            #TODO Why not just put .propagate in self?
            propagator.propagate(self)

    def new_state(self, node, state):
        datum = self.datum(node)
        try:
            datum.state = state
            if state.is_completed:
                datum.on_completion(self, node)
        except AttributeError:
            pass
