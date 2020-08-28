# testTimeStepper.py -- Unit tests for TimeStepper
#
# These tests also show how to make and run a minimal FARG model.

import unittest

from TimeStepper import TimeStepper
from PortGraph import PortGraph, Node, pg
from bases import ActiveNode, Action
from NodeParams import NodeParams, MateParam, AttrParam
from log import ShowActiveNodes, ShowActionList, ShowActionsChosen, \
        stop_all_logging

stop_all_logging()

# Node definitions for TestGraph

class Seeker(ActiveNode):
    '''A Seeker seeks a Sought with the same .value.'''
    node_params = NodeParams(AttrParam('value'))

    # Required: Returns list of Action objects
    def actions(self, g, thisid):
        return [MakeLink(thisid, nodeid)
                    for nodeid in g.nodes_of_class(Sought)
                        if self.am_seeking(g, nodeid)]

    # Override: Tells when to stop polling this node for actions
    def dormant(self, g, thisid):
        return g.has_neighbor_at(thisid, 'found')

    def am_seeking(self, g, nodeid):
        return (
            not g.has_neighbor_at(nodeid, 'seeker')
            and
            g.value_of(nodeid) == self.value
        )

class Sought(Node):
    '''A Sought passively (without generating Actions) awaits a Seeker.'''
    node_params = NodeParams(AttrParam('value'))

# Action definitions

class MakeLink(Action):

    def __init__(self, thisid, soughtid):
        self.on_behalf_of = thisid
        self.soughtid = soughtid

    # Required override: performs the action
    def go(self, g):
        g.add_edge(self.on_behalf_of, 'found', self.soughtid, 'seeker')


# This is all you have to do to make a FARG model

class TestGraph(TimeStepper, PortGraph):
    pass

class TestTimeStepper(unittest.TestCase):

    def test_time_stepper(self):
        #ShowActiveNodes.start_logging()
        #ShowActionList.start_logging()
        #ShowActionsChosen.start_logging()
        g = TestGraph()
        g.make_node(Sought(1))
        g.make_node(Sought(2))
        g.make_node(Seeker(1))
        g.make_node(Seeker(2))
        self.assertEqual(len(g.edges()), 0)
        g.do_timestep()
        self.assertEqual(len(g.edges()), 1) # first Seeker found a mate
        g.do_timestep()
        self.assertEqual(len(g.edges()), 2) # second Seeker found a mate
        g.do_timestep()
        self.assertEqual(len(g.edges()), 2) # nothing left to do
        #pg(g)

    def test_do_specified_action(self):
        # Forcing a specified Action by passing it to do_timestep().
        g = TestGraph(seed=1)
        for i in range(100):
            g.make_node(Sought(100))
            g.make_node(Seeker(100))
        sought2 = g.make_node(Sought(2))
        seeker2 = g.make_node(Seeker(2))
        g.do_timestep(action=MakeLink(seeker2, sought2))
        self.assertTrue(g.has_hop(seeker2, 'found', sought2, 'seeker'))
