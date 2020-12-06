import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Callable

from Ac import Ac, AcNode, All, AllAre, TagWith
from codegen import make_python, compile_fargish
from criteria import OfClass, Tagged as CTagged
from StdGraph import Graph, pg 
from Numble import make_numble_class
from testNodeClasses import *
from Node import Node, NRef
from ActiveNode import ActiveNode, Start, Completed
from log import *
from util import first
from exc import AcNeedArg

prog = '''
tags -- taggees

Workspace

Tag(taggees)
AllBricksAvail, Blocked(reason) : Tag

Group(members)
Glom : Group
'''
exec(compile_fargish(prog), globals())

Numble = make_numble_class(
    Brick, Target, Want, Avail, Allowed, [Plus, Times]
)

class TestGraph(Graph):

    def __init__(self, numble, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.nodeclasses.update(nodeclasses)
        self.port_mates += port_mates
        ws = self.add_node(Workspace)
        numble.build(self, ws)

class TestAc(unittest.TestCase):

    def test_do_notice_and_tag(self):
        g = TestGraph(Numble([4, 5, 6], 15))
        targetid = 1  # HACK

        env = Ac.run(
            g,
            All(OfClass(Brick), within=g.ws),
            actor=targetid
        )
        nodes = map(g.as_node, env['nodes'])
        self.assertCountEqual(nodes, [Brick(4), Brick(5), Brick(6)])

        # Should fail because All is missing a 'within' arg.
        try:
            env = Ac.run(g, All(OfClass(Brick)), actor=targetid)
        except AcNeedArg as exc:
            self.assertEqual(
                exc, AcNeedArg(ac=All(OfClass(Brick)), name='within')
            )
        else:
            self.fail("Missing 'within' failed to raise AcNeedArg.")

        env = Ac.run(g, [
            All(OfClass(Brick), within=g.ws),
            AllAre(CTagged(Avail))
        ], actor=targetid)
        nodes = map(g.as_node, env['nodes'])
        self.assertCountEqual(nodes, [Brick(4), Brick(5), Brick(6)])

        env = Ac.run(g, [
            All(OfClass(Brick), within=g.ws),
            AllAre(CTagged(Avail)),
            TagWith(AllBricksAvail)
        ], actor=targetid)
        result = list(g.as_nodes(env['result']))

        self.assertEqual(len(result), 1)
        tag = result[0]
        self.assertEqual(tag.__class__, AllBricksAvail)
        self.assertTrue(g.has_tag(nodes, tag))

    def test_noticer(self):
        g = TestGraph(Numble([4, 5, 6], 15))
        
        noticer = g.add_node(AcNode, [
            All(OfClass(Brick), within=g.ws),
            AllAre(CTagged(Avail)),
            TagWith(AllBricksAvail)
        ], member_of=g.ws)

        #pg(g)
        #ShowActiveNodes.start_logging()
        #ShowActiveNodesCollected.start_logging()
        #ShowActionList.start_logging()
        #ShowActionsChosen.start_logging()

        #g.do_timestep()
        #print('STATE', noticer.state, noticer.can_go())
        self.assertEqual(noticer.state, Start)
        self.assertTrue(noticer.can_go())
        self.assertIn(noticer.id, g.as_nodeids(g.active_nodes()))
        g.do_timestep(actor=noticer)
        tag = g.as_node(first(g.new_nodes))
        self.assertEqual(tag.__class__, AllBricksAvail)
        self.assertEqual(noticer.state, Completed)
        self.assertFalse(noticer.can_go())
        self.assertNotIn(noticer.id, g.as_nodeids(g.active_nodes()))

    def test_override(self):
        g = TestGraph(Numble([4, 5, 6], 15))
        glom = g.add_node(Glom, g.find_all(OfClass(Brick)))

        noticer = g.add_node(AcNode, [
            All(OfClass(Brick)),
            AllAre(CTagged(Avail)),
            TagWith(AllBricksAvail)
        ], name='Noticer', member_of=g.ws)

        self.assertIn(noticer.id, g.as_nodeids(g.active_nodes()))
        g.do_timestep(actor=noticer)
        self.assertTrue(g.has_tag(noticer, Blocked))

        self.assertNotIn(noticer.id, g.as_nodeids(g.active_nodes()))

        g.add_override_node(noticer, 'within', glom)
        g.remove_tag(noticer, Blocked)

        g.do_timestep(actor=noticer)
        bricks = g.find_all(OfClass(Brick))
        self.assertTrue(
            g.has_tag(bricks, AllBricksAvail),
            "Did not tag the Bricks even when 'within' was overridden."
        )
