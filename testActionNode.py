# testActionNode.py -- Unit tests for ActionNode and related classes

import unittest
from pprint import pprint as pp

from Action import Action
from ActiveNode import ActionNode, Start, Dormant, Completed
from PortGraph import PortGraph, Node, pg
from log import *
from TimeStepper import TimeStepper
import inspect

class MyAction(Action):

    def go(self, g):
        try:
            g.graph['MyAction_ran'] += 1
        except KeyError:
            g.graph['MyAction_ran'] = 1
        g.new_state(self.actor, Completed)


class TestGraph(TimeStepper, PortGraph):
    pass


#ShowActiveNodes.start_logging()
#ShowActionList.start_logging()
#ShowActionsChosen.start_logging()

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

if __name__ == '__main__':
    g = TestGraph()
    node = g.make_node(ActionNode, **dict(action=MyAction))
