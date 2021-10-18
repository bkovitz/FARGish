# testWorkspace.py -- Unit tests for Workspace.py

import unittest
from pprint import pprint as pp
import inspect

from Workspace import Workspace

class TestWorkspace(unittest.TestCase):

    def test_workspace_basics(self):
        ws = Workspace()

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
