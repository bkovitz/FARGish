# testCubeCanvas.py -- Unit tests for CubeCanvas.py

import unittest
from pprint import pprint as pp
import inspect

from FARGModel import Addr
from CubeCanvas import CubeCanvas, Horiz, Vert, Diag
from util import ps, pss, pr, empty_set


class TestCubeCanvas(unittest.TestCase):

    def test_jump(self) -> None:
        cc = CubeCanvas()
        for i in range(1, 9):
            for relation in (Horiz, Vert, Diag):
                # jumping across the same kind of line twice should return
                # you to the original corner
                addr1 = Addr(cc, None, i)
                addr2 = cc.jump_or_fizzle(addr1, relation)
                addr3 = cc.jump_or_fizzle(addr2, relation)
                self.assertNotEqual(addr2, addr1)
                self.assertEqual(addr3, addr1)

    def test_set_get(self) -> None:
        cc = CubeCanvas()
        addr = Addr(cc, None, 1)
        self.assertEqual(cc[addr], None)
        cc[addr] = 0
        self.assertEqual(cc[addr], 0)
        cc[addr] = 1
        self.assertEqual(cc[addr], 1)
