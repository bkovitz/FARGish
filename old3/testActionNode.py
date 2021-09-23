# testActionNode.py -- Unit tests for ActionNode and related classes

import unittest
from pprint import pprint as pp
import inspect

from Action import Action
from ActiveNode import ActionNode, ActionSeqNode, Start, Dormant, Completed, \
    make_action_sequence
from StdGraph import Graph, pg
from log import *
from util import reseed


class MyAction(Action):

    def go(self, g, actor):
        try:
            g.MyAction_ran += 1
        except AttributeError:
            g.MyAction_ran = 1
        g.new_state(actor, Completed)

class FirstAction(Action):
    threshold = 1.0

    def go(self, g, actor):
        try:
            g.Actions += 'First'
        except AttributeError:
            g.Actions = 'First'
        g.new_state(actor, Completed)

class SecondAction(Action):
    threshold = 1.0

    def go(self, g, actor):
        try:
            g.Actions += 'Second'
        except AttributeError:
            g.Actions = 'Second'
        g.new_state(actor, Completed)

class ThirdAction(Action):
    threshold = 1.0

    def go(self, g, actor):
        try:
            g.Actions += 'Third'
        except AttributeError:
            g.Actions = 'Third'
        g.new_state(actor, Completed)

class WriteString(Action):

    def go(self, g, actor):
        g.Actions = self.get_kwarg('string')
        g.new_state(actor, Completed)


class TestGraph(Graph):
    def __init__(self, *args, **kwargs):
        if 'seed' not in kwargs:
            kwargs['seed'] = 1
        super().__init__(*args, **kwargs)


#ShowActiveNodes.start_logging()
#ShowActionList.start_logging()
#ShowActionsChosen.start_logging()

class TestActionNode(unittest.TestCase):
    
    def test_simplest_action_node(self):
        g = TestGraph()
        node = g.add_node(ActionNode, action=MyAction())
        # TODO Catch easy bug: passing the Action class instead of an Action
        # object.
        self.assertEqual(g.value_of(node, 'state'), Start)
        g.do_timestep()
        self.assertEqual(g.value_of(node, 'state'), Completed)
        self.assertEqual(g.MyAction_ran, 1)

    def test_new_state_on_nonexistent_node(self):
        g = TestGraph()
        g.new_state(1, Completed)


class TestActionSequence(unittest.TestCase):

    def test_make_action_sequence(self):
        g = TestGraph()
        seqnode = make_action_sequence(
            g, FirstAction(), SecondAction(), ThirdAction(), min_activation=5.0
        )
        g.set_activation(seqnode, 10.0)
        #g.do_timestep(num=20)
        for i in range(20):
            g.do_timestep()
            self.no_more_than_one_activated(g, seqnode)
        #pg(g)
        self.assertTrue(hasattr(g, 'Actions'), 'No ActionNode ever fired.')
        self.assertEqual(g.Actions, 'FirstSecondThird',
            'ActionNodes fired in wrong sequence.'
        )

    def no_more_than_one_activated(self, g, seqnode):
        active_members = [
            node
                for node in g.members_of(seqnode)
                    if not g.is_dormant(node) and g.activation(node) >= 1.0
        ]
        #pg(g)
        self.assertLessEqual(len(active_members), 1,
            f'More than one ActionNode is active: {active_members}'
        )


if __name__ == '__main__':
    pass
