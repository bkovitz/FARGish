# testActionNode.py -- Unit tests for ActionNode and related classes

import unittest
from pprint import pprint as pp
import inspect

from Action import Action
from ActiveNode import ActionNode, ActionSeqNode, Start, Dormant, Completed, \
    make_action_sequence
from PortGraph import PortGraph, Node, pg
from log import *
from TimeStepper import TimeStepper
import support
from util import reseed


class MyAction(Action):

    def go(self, g):
        try:
            g.graph['MyAction_ran'] += 1
        except KeyError:
            g.graph['MyAction_ran'] = 1
        g.new_state(self.actor, Completed)

class FirstAction(Action):
    threshold = 1.0

    def go(self, g):
        try:
            g.graph['Actions'] += 'First'
        except KeyError:
            g.graph['Actions'] = 'First'
        g.new_state(self.actor, Completed)

class SecondAction(Action):
    threshold = 1.0

    def go(self, g):
        try:
            g.graph['Actions'] += 'Second'
        except KeyError:
            g.graph['Actions'] = 'Second'
        g.new_state(self.actor, Completed)

class ThirdAction(Action):
    threshold = 1.0

    def go(self, g):
        try:
            g.graph['Actions'] += 'Third'
        except KeyError:
            g.graph['Actions'] = 'Third'
        g.new_state(self.actor, Completed)

class WriteString(Action):

    def go(self, g):
        g.graph['Actions'] = self.get_kwarg('string')
        g.new_state(self.actor, Completed)


class TestGraph(TimeStepper, PortGraph):
    default_graph_attrs = dict(
        seed=1,
        num_timesteps=40,
        support_propagator=support.Propagator(
            max_total_support=20,
            positive_feedback_rate=0.1,
            sigmoid_p=0.5,
            alpha=0.98
        )
    )

    def __init__(self, **kwargs):
        super().__init__()
        kws = self.default_graph_attrs.copy()
        kws.update(kwargs)
        if kws.get('num_timesteps', None) is None:
            kws['num_timesteps'] = self.default_graph_attrs['num_timesteps']
        kws['seed'] = reseed(kws.get('seed', None))
        super().__init__(**kws)
        self.consecutive_timesteps_with_no_response = 0


#ShowActiveNodes.start_logging()
ShowActionList.start_logging()
ShowActionsChosen.start_logging()

class TestActionNode(unittest.TestCase):
    
    def test_simplest_action_node(self):
        g = TestGraph()
        node = g.make_node(ActionNode, action=MyAction())
        # TODO Catch easy bug: passing the Action class instead of an Action
        # object.
        self.assertEqual(g.value_of(node, 'state'), Start)
        g.do_timestep()
        self.assertEqual(g.value_of(node, 'state'), Completed)
        self.assertEqual(g.graph['MyAction_ran'], 1)

    def test_new_state_on_nonexistent_node(self):
        g = TestGraph()
        g.new_state(1, Completed)

    def test_action_kwarg(self):
        g = TestGraph()
        g.graph['Actions'] = 'NOT DONE'
        node = g.make_node(ActionNode, action=WriteString())
        g.do_timestep()
        pg(g)
        print(g.graph['Actions'])


class TestActionSequence(unittest.TestCase):

    def test_simple_action_sequence(self):
        g = TestGraph()
        anode1 = g.make_node(ActionNode, action=FirstAction())
        anode2 = g.make_node(ActionNode, action=SecondAction())
        anode3 = g.make_node(ActionNode, action=ThirdAction())
        seqnode = g.make_node(
            ActionSeqNode,
            action_nodes=[anode1, anode2, anode3],
            members=[anode1, anode2, anode3],
            min_support_for=5.0  # HACK Without this, support for seqnode falls
                                 # too low to activate the last couple
                                 # ActionNodes.
        )
        g.set_support_for(seqnode, 10.0)
        g.do_timestep(num=20)
        self.assertEqual(g.graph['Actions'], 'FirstSecondThird')

    def test_make_action_sequence(self):
        g = TestGraph()
        seqnode = make_action_sequence(
            g, FirstAction(), SecondAction(), ThirdAction(), min_support_for=5.0
        )
        g.set_support_for(seqnode, 10.0)
        g.do_timestep(num=20)
        self.assertEqual(g.graph['Actions'], 'FirstSecondThird')


if __name__ == '__main__':
    pass
