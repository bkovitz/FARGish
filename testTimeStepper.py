# testTimeStepper.py -- Unit tests for TimeStepper
#
# These tests also show how to make and run a minimal FARG model.

import unittest
from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Callable

from TimeStepper import TimeStepper
#from PortGraph import PortGraph, Node, pg
from StdGraph import Graph, pg
from Node import Node, MaybeNRef, CRef
from Action import Action
from ActiveNode import ActiveNode
from NodeParams import NodeParams, MateParam, AttrParam
from log import *

stop_all_logging()

# Node definitions for TestGraph

class Seeker(ActiveNode):
    '''A Seeker seeks a Sought with the same .value.'''
    node_params = NodeParams(AttrParam('value'))

    # Required: Returns list of Action objects
    def actions(self, g):
        return [MakeLink(self, nodeid)
                    for nodeid in g.nodes_of_class(Sought)
                        if self.am_seeking(g, nodeid)]

    # Override: Tells when to stop polling this node for actions
    def dormant(self, g):
        return g.has_neighbor_at(self, 'found')

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

@dataclass
class MakeLink(Action):
    on_behalf_of: MaybeNRef
    soughtid: MaybeNRef

    def go(self, g):
        g.add_edge(self.on_behalf_of, 'found', self.soughtid, 'seeker')

@dataclass
class MakeNode(Action):
    nodeclass: CRef
    args: Tuple
    kwargs: Dict

    def __init__(self, nodeclass, *args, **kwargs):
        self.nodeclass = nodeclass
        self.args = args
        self.kwargs = kwargs
        super().__init__()

    def go(self, g):
        g.add_node(self.nodeclass, *self.args, **self.kwargs)


# This is all you have to do to make a FARG model

class TestGraph(Graph):
    pass

class TestTimeStepper(unittest.TestCase):

    def setUp(self):
        stop_all_logging()

    def test_time_stepper(self):
        g = TestGraph()
        g.add_node(Sought(1))
        g.add_node(Sought(2))
        g.add_node(Seeker(1))
        g.add_node(Seeker(2))
        self.assertEqual(g.num_edges(), 0)
        g.do_timestep()
        self.assertEqual(g.num_edges(), 1) # first Seeker found a mate
        g.do_timestep()
        self.assertEqual(g.num_edges(), 2) # second Seeker found a mate
        g.do_timestep()
        self.assertEqual(g.num_edges(), 2) # nothing left to do
        #pg(g)

    def test_do_specified_action(self):
        # Forcing a specified Action by passing it to do_timestep().
        g = TestGraph(seed=1)
        for i in range(100):
            g.add_node(Sought(100))
            g.add_node(Seeker(100))
        sought2 = g.add_node(Sought(2))
        seeker2 = g.add_node(Seeker(2))
        g.do_timestep(action=MakeLink(seeker2, sought2))
        self.assertTrue(g.has_hop(seeker2, 'found', sought2, 'seeker'))
        self.assertEqual(g.t, 1)

    def test_do_action_sequence(self):
        # Forcing a sequence of Actions, one per timestep.
        g = TestGraph(seed=1)
        for i in range(100):
            g.add_node(Sought(100))
            g.add_node(Seeker(100))
        sought2 = g.add_node(Sought(2))
        seeker2 = g.add_node(Seeker(2))
        sought3 = g.add_node(Sought(3))
        seeker3 = g.add_node(Seeker(3))
        sought4 = g.add_node(Sought(4))
        seeker4 = g.add_node(Seeker(4))
        g.do_actions1([
            MakeLink(seeker2, sought2),
            MakeLink(seeker3, sought3),
            MakeLink(seeker4, sought4),
        ])
        self.assertTrue(g.has_hop(seeker2, 'found', sought2, 'seeker'))
        self.assertTrue(g.has_hop(seeker3, 'found', sought3, 'seeker'))
        self.assertTrue(g.has_hop(seeker4, 'found', sought4, 'seeker'))
        self.assertEqual(g.t, 3)

    def test_new_nodes(self):
        g = TestGraph(seed=1)
        g.do_timestep(action=MakeNode(Sought, 2))
        self.assertEqual(len(g.new_nodes), 1)
        self.assertEqual(len(g.prev_new_nodes), 0)
        new_node = list(g.new_nodes)[0]
        self.assertEqual(g.class_of(new_node), Sought)
        self.assertEqual(g.value_of(new_node), 2)
