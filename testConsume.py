# testConsume.py -- Unit tests of consuming operands and making result nodes

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable

from testNumboClasses import *
from Node import Node, NRef, NRefs, CRef, MaybeNRef, as_nodeid
from log import *

class TestConsume(unittest.TestCase):
    
    def setUp(self):
        stop_all_logging()

    def test_plus(self):
        g = NumboTestGraph(Numble([4, 5, 6], 15))
        brick4 = g.look_for(Brick(4))
        brick5 = g.look_for(Brick(5))
        assert g.has_tag(brick4, Avail)
        assert g.has_tag(brick5, Avail)

        #g.consume_operands([brick4, brick5], Plus)
        g.consume_operands(Plus, operands=[brick4, brick5])

        plus = g.look_for(Plus, subset=g.neighbors(brick4))
        self.assertTrue(plus)
        block9 = g.look_for(Block(9))
        self.assertTrue(block9)
        self.assertTrue(g.has_tag(block9, Avail))
        self.assertFalse(g.has_tag(brick4, Avail))
        self.assertFalse(g.has_tag(brick5, Avail))
        self.assertTrue(g.is_member([plus, block9], g.ws))

    def test_times(self):
        g = NumboTestGraph(Numble([4, 5, 6], 15))
        brick4 = g.look_for(Brick(4))
        brick5 = g.look_for(Brick(5))
        assert g.has_tag(brick4, Avail)
        assert g.has_tag(brick5, Avail)

        g.consume_operands(Times, operands=[brick4, brick5])

        times = g.look_for(Times, subset=g.neighbors(brick4))
        self.assertTrue(times)
        block20 = g.look_for(Block(20))
        self.assertTrue(block20)
        self.assertTrue(g.has_tag(block20, Avail))
        self.assertFalse(g.has_tag(brick4, Avail))
        self.assertFalse(g.has_tag(brick5, Avail))

    def test_minus(self):
        g = NumboTestGraph(Numble([4, 9, 12], 17))
        brick4 = g.look_for(Brick(4))
        brick9 = g.look_for(Brick(9))
        assert g.has_tag(brick4, Avail)
        assert g.has_tag(brick9, Avail)

        g.consume_operands(Minus, minuend=brick9, subtrahend=brick4)

        minus = g.look_for(Minus, subset=g.neighbors(brick4))
        self.assertTrue(minus)
        block5 = g.look_for(Block(5))
        self.assertTrue(block5)
        self.assertTrue(g.has_tag(block5, Avail))
        self.assertFalse(g.has_tag(brick4, Avail))
        self.assertFalse(g.has_tag(brick9, Avail))
