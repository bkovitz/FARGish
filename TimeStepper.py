# TimeStepper.py -- Mix-in class to run a timestep in a FARG model
#
# The main method, .do_timestep(), chooses nodes for activity, queries them
# for Actions, chooses Actions, and runs them.

from operator import attrgetter

from PortGraph import pg
from bases import ActiveNode, CoarseView
from util import sample_without_replacement
from log import ShowActiveNodes, ShowActionList, ShowActionsChosen, ShowResults


class TimeStepper:
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
        super().__init__(*args, **kws)

    def do_timestep(self, num=1):
        '''Executes n timesteps.

        On each timestep, we decay saliences, choose active nodes, generate
        Action objects from the active nodes, choose which Actions to
        perform, execute those Actions, and update support.
        '''

        for i in range(num):
            self.graph['t'] += 1

            self.decay_saliences()

            self.update_coarse_views()

            active_nodes = self.get_active_nodes()

            if (self.max_active_nodes is not None
                and
                len(active_nodes) > self.max_active_nodes
            ):
                active_nodes = self.choose_active_nodes(active_nodes)
            if ShowActiveNodes.is_logging():
                print('ACTIVE NODES')
                pg(self, active_nodes)

            actions = self.collect_actions(active_nodes)
            if len(actions) == 0:
                self.consecutive_timesteps_with_no_action += 1
                #TODO Stop or do something if idle too long
            else:
                self.consecutive_timesteps_with_no_action = 0
            if ShowActionList.is_logging():
                print('ACTIONS COLLECTED')
                for action in sorted(actions, key=attrgetter('weight')):
                    print('  %.3f (%.3f) %s' % (
                        action.weight,
                        action.threshold,
                        action
                    ))

            chosen_actions = self.choose_actions(actions)
            if ShowActionsChosen.is_logging():
                print('ACTIONS')
                #TODO OAOO with above
                for action in sorted(chosen_actions, key=attrgetter('weight')): 
                    print('  %.3f (%.3f) %s' % (
                        action.weight,
                        action.threshold,
                        action
                    ))

            for action in chosen_actions:
                self.do_action(action)

            self.update_all_support()

            d = self.done()
            if d:
                ShowResults(d)
                break

    def do_action(self, action):
        '''action: an Action object'''
        action.go(self)

    def set_done(self, done):
        self.graph['done'] = done

    def done(self):
        try:
            return self.graph['done']
        except KeyError:
            return False

    def get_active_nodes(self):
        '''Must return a collection of nodes.'''
        return list(node for node in self.nodes_of_class(ActiveNode)
                             if not self.datum(node).dormant(self, node))

    def choose_active_nodes(self, active_nodes, k=None):
        '''Randomly chooses up to k Actions, weighted by .weight.
        Returns a collection. k defaults to self.max_active_nodes.'''
        if k is None:
            k = self.max_active_nodes
        return list(sample_without_replacement(
            active_nodes,
            k=k,
            weights=[self.salience(node) for node in active_nodes]
        ))

    def collect_actions(self, active_nodes):
        '''Calls .look() on each node in active_nodes, and returns all the
        returned objects (presumed to be Actions) in a list. Filters out
        all Action objects whose .weight doesn't match or exceed their
        .threshold.'''
        actions = []
        for node in active_nodes:
            for action in self.datum(node).actions(self, node):
                if (action is not None
                    and
                    action.weight >= action.threshold
                ):
                    actions.append(action)
        return actions

    def choose_actions(self, actions, k=None):
        '''Randomly chooses up to k Actions, weighted by .weight.
        Returns a collection. k defaults to self.max_actions.'''
        if k is None:
            k = self.max_actions
        return list(sample_without_replacement(
            actions, k=k, weights=[a.weight for a in actions]
        ))

    def update_coarse_views(self):
        for nodeid in self.nodes_of_class(CoarseView):
            self.datum(nodeid).update(self, nodeid)
