# testCodegen.py -- Tests of the FARGish grammar and compilation to Python

import unittest
from pprint import pprint as pp

from codegen import make_python, compile_fargish
from PortGraph import PortGraph, Node, pg

class TestCodegen(unittest.TestCase):

    def testOneSimpleNode(self):
        g = PortGraph()
        prog = '''
SomeNode
'''
        exec(compile_fargish(prog), globals())
        # The globals() argument makes exec put the newly defined classes
        # into our globals. Python does not provide a way (that works) for
        # exec to add classes to our local variables.
        g.make_node(SomeNode)
        got = g.all_datums()
        expect = [SomeNode()]
        self.assertCountEqual(got, expect)

    def testNodeWithArg(self):
        g = PortGraph()
        prog = '''
Number(n)
Brick : Number'''
        make_python(prog)
        exec(compile_fargish(prog), globals())
        g.make_node(Brick(2))
        got = g.all_datums()
        expect = [Brick(n=2)]
        self.assertCountEqual(got, expect)
