import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Callable

from Ac import Ac, AcNode, All, AllAre, TagWith
from criteria import OfClass, Tagged
from StdGraph import Graph, pg 
from Numble import make_numble_class
from testNodeClasses import *
from Node import Node, NRef
from ActiveNode import ActiveNode
from log import *
from util import first


class Workspace(Node):
    pass

class AllBricksAvail(Node):
    pass

Numble = make_numble_class(
    Brick, Target, Want, Avail, Allowed, [Plus, Times]
)

class TestAc(unittest.TestCase):

    def test_do_notice_and_tag(self):
        g = Graph(port_mates=port_mates)
        ws = g.add_node(Workspace)
        numble = Numble([4, 5, 6], 15)
        numble.build(g, ws)
        targetid = 1  # HACK

#        find_all_bricks = Acs(
#            All(OfClass(Brick), within=ws)
#        )
#
#        find_that_all_bricks_are_avail = Acs(
#            All(OfClass(Brick), within=ws),
#            AllAre(Tagged(Avail))
#        )
#
#        notice_and_tag = Acs(
#            All(OfClass(Brick), within=ws),
#            AllAre(Tagged(Avail)),
#            TagWith(AllBricksAvail)
#        )

        #env, result = find_that_all_bricks_are_avail.do(g, targetid, empty_env)
        #self.assertTrue(result)

        env = Ac.run(
            g,
            All(OfClass(Brick), within=ws),
            actor=targetid
        )
        bricks = map(g.as_node, env['nodes'])
        self.assertCountEqual(bricks, [Brick(4), Brick(5), Brick(6)])

        env = Ac.run(g, [
            All(OfClass(Brick), within=ws),
            AllAre(Tagged(Avail))
        ], actor=targetid)
        bricks = map(g.as_node, env['nodes'])
        self.assertCountEqual(bricks, [Brick(4), Brick(5), Brick(6)])

        #env, result = notice_and_tag.do(g, targetid, empty_env)
        env = Ac.run(g, [
            All(OfClass(Brick), within=ws),
            AllAre(Tagged(Avail)),
            TagWith(AllBricksAvail)
        ], actor=targetid)
        result = list(g.as_nodes(env['result']))

        #pg(g) #DEBUG
        #print('ENV', env)
        #print('RESULT', result)

        self.assertEqual(len(result), 1)
        tag = result[0]
        self.assertEqual(tag, AllBricksAvail())
        self.assertTrue(g.has_tag(bricks, tag))

    def test_noticer(self):
        g = Graph(port_mates=port_mates)
        ws = g.add_node(Workspace)
        numble = Numble([4, 5, 6], 15)
        numble.build(g, ws)
        
#        notice_and_tag = Acs(
#            All(OfClass(Brick), within=ws),
#            AllAre(Tagged(Avail)),
#            TagWith(AllBricksAvail)
#        )
#
#        @dataclass
#        class DoAc(Action):
#            ac: Ac
#
#            def go(self, g):
#                self.ac.run(g, self.actor)
#
#        class Noticer(ActiveNode):
#            def actions(self, g):
#                return DoAc(notice_and_tag)

        #noticer = g.add_node(Noticer, member_of=ws)
        noticer = g.add_node(AcNode, [
            All(OfClass(Brick), within=ws),
            AllAre(Tagged(Avail)),
            TagWith(AllBricksAvail)
        ])

        #pg(g)
        #ShowActiveNodes.start_logging()
        #ShowActiveNodesCollected.start_logging()
        #ShowActionList.start_logging()
        #ShowActionsChosen.start_logging()

        #g.do_timestep()
        g.do_timestep(actor=noticer)
        tag = g.as_node(first(g.new_nodes))
        self.assertEqual(tag, AllBricksAvail())

#if __name__ == '__main__':
