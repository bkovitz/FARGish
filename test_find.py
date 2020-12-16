# test_find.py -- Unit tests for ActiveGraph.look_for() and .find_all()

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable

from StdGraph import Graph, MyContext, InWorkspace, pg
from testNumboClasses import *
from criteria import OfClass, Tagged as CTagged, NotTheArgsOf, And
from Node import Node, NodeId, NRef, NRefs, CRef, MaybeNRef, as_nodeid
from log import *

class TestFind(unittest.TestCase):

    def test_find_ofclass(self):
        g = NumboTestGraph(Numble([4, 5, 6], 15))
        brickids = g.find_all(OfClass(Brick), within=g.ws)
        expected = [Brick(4), Brick(5), Brick(6)]
        self.assertCountEqual(g.as_nodes(brickids), expected)

        brick = g.as_node(g.look_for(OfClass(Brick)))
        self.assertIn(brick, expected)

    def test_find_class(self):
        g = NumboTestGraph(Numble([4, 5, 6], 15))
        brickids = g.find_all(Brick, within=g.ws)
        expected = [Brick(4), Brick(5), Brick(6)]
        self.assertCountEqual(g.as_nodes(brickids), expected)

        brick = g.as_node(g.look_for(Brick))
        self.assertIn(brick, expected)

    def test_find_by_model_node(self):
        g = NumboTestGraph(Numble([4, 5, 6], 15))
        b4 = g.look_for(Brick(4))
        b5 = g.look_for(Brick(5))
        self.assertEqual(g.as_node(b4), Brick(4))
        self.assertEqual(g.as_node(b5), Brick(5))

    def test_and_criteria(self):
        class TestTag(Tag):
            pass
        g = NumboTestGraph(Numble([4, 5, 6], 15))
        b4 = g.look_for(Brick(4))
        g.add_tag(TestTag, b4)
        
        self.assertCountEqual(
            g.find_all(And(OfClass(Brick), CTagged(TestTag))),
            [b4]
        )

    def test_find_cartesian_product(self):
        g = NumboTestGraph(Numble([4, 5, 6], 15))
        b4 = g.look_for(Brick(4))
        b5 = g.look_for(Brick(5))
        b6 = g.look_for(Brick(6))
        brickpairs = g.find_all([Brick, Brick], within=g.ws)
        self.assertCountEqual(
            brickpairs,
            [
                (b4, b5), (b4, b6), (b5, b6),
                (b5, b4), (b6, b4), (b6, b5)
            ]
        )

        plus = g.add_node(Plus, operands=[b4, b5])
        brickpairs = g.find_all(
            [Brick, Brick],
            tupcond=NotTheArgsOf(Plus, 'operands')
        )
        expect = [
            (b4, b6), (b5, b6),  # brick pairs that are not the operands
            (b6, b4), (b6, b5)   # of plus.
        ]
        self.assertCountEqual(brickpairs, expect)

        pair: Tuple[NodeId] = g.look_for(
            [Brick, Brick],
            tupcond=NotTheArgsOf(Plus, 'operands')
        )
        self.assertIn(pair, expect)
