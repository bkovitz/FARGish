# testAction.py -- Unit tests for Action.py

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Callable

from StdGraph import Graph, pg 
from Action import Action
from Node import Node, NRef


@dataclass
class SetAttr(Action):
    name: str
    v: Any

    def go(self, g: Graph, actor: NRef):
        g.set_attr(actor, self.name, self.v)

class TestAction(unittest.TestCase):

    def test_simple_action(self):
        g = Graph()
        node = g.add_node(Node)
        self.assertIs(g.value_of(node, 'attr'), None)
        g.do_action(SetAttr('attr', 1), node)
        self.assertEqual(g.value_of(node, 'attr'), 1)
