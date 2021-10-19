# testWorkspace.py -- Unit tests for Workspace.py

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass, field, fields, replace, is_dataclass, InitVar
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, \
    Iterator, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable
from collections import Counter

from FARGModel import Workspace
from FMTypes import match_wo_none
from Agents import Agent
from util import first, pr


@dataclass(frozen=True)
class Obj:
    a: Optional[str] = None
    b: Optional[int] = None

@dataclass(frozen=True)
class MyBuilder(Agent):
    pass

class TestWorkspace(unittest.TestCase):

    def test_workspace_basics(self) -> None:
        ws = Workspace()

        self.assertIsInstance(ws.seed, int)

        # build a node
        one = ws.build(1)
        self.assertEqual(one, 1)
        self.assertCountEqual(ws.nodes(), [1])
        self.assertEqual(ws.the(int), 1)
        self.assertEqual(ws.the(1), 1)
        self.assertEqual(ws.a(1), 1.0)

        # non-existent node
        self.assertEqual(ws.the(2), None)
        self.assertEqual(ws.a(2), 0.0)

    def test_optional_attrs(self) -> None:
        ws = Workspace()

        builder = ws.build(MyBuilder())
        node = ws.build(
            Obj('abc'),
            builder=builder,
            init_a=0.8,
            min_a=0.2,
            max_a=1.2
        )

        self.assertEqual(ws.builder_of(builder), None)
        self.assertEqual(ws.builder_of(node), builder)

        self.assertEqual(ws.a(node), 0.8)
        self.assertEqual(node.min_a, 0.2)  # type: ignore[attr-defined]
        self.assertEqual(node.max_a, 1.2)  # type: ignore[attr-defined]

    def test_double_build(self) -> None:
        # Building the same node twice only builds it once.
        ws = Workspace()
        node1a = ws.build(Obj('abc', 1))
        node1b = ws.build(Obj('abc', 1))
        self.assertIs(node1a, node1b)

    def test_match_wo_none(self) -> None:
        oboth1 = Obj('xyz', 1)
        oboth2 = Obj('xyz', 2)
        ojust1 = Obj(None, 1)
        oneither = Obj()

        self.assertTrue(match_wo_none(oboth1, oboth1))
        self.assertFalse(match_wo_none(oboth1, oboth2))

        self.assertTrue(match_wo_none(oboth1, ojust1))
        self.assertFalse(match_wo_none(ojust1, oboth1))

        self.assertTrue(match_wo_none(oboth1, oneither))
        self.assertFalse(match_wo_none(oneither, oboth1))

        self.assertFalse(match_wo_none(oboth2, ojust1))
        self.assertFalse(match_wo_none(ojust1, oboth2))

    def test_ws_nodes(self) -> None:
        ws = Workspace()
        o1 = ws.build(Obj('abc', 1))
        o2 = ws.build(Obj('abc', 2))
        o3 = ws.build(Obj('xyz', 3))
        i1 = ws.build(1)
        i2 = ws.build(2)
        self.assertCountEqual(ws.nodes(Obj), [o1, o2, o3])
        self.assertCountEqual(ws.nodes(Obj('abc')), [o1, o2])
        self.assertEqual(ws.the(Obj('xyz')), o3)

    def test_ws_query(self) -> None:
        ws = Workspace(seed=1)
        self.assertEqual(ws.seed, 1)

        o1 = ws.build(Obj('abc', 1), init_a=10.0)
        o2 = ws.build(Obj('abc', 2))
        o3 = ws.build(Obj('xyz', 3))
        i1 = ws.build(1)
        i2 = ws.build(2)
        results = Counter([
            ws.ws_query1(Obj)
                for _ in range(100)
        ])
        self.assertGreater(results[o1], results[o2])
        self.assertEqual(results[i1], 0)
        self.assertEqual(results[i2], 0)
