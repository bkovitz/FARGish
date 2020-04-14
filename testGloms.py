# testGloms.py -- Unit test to verify that gloms work

import unittest

from PortGraph import PortGraph, pg, ps
from TimeStepper import TimeStepper
from codegen import make_python, compile_fargish
from log import stop_all_logging


prog = '''
postamble { gloms }

glom -- glommees

Glom

Number(value)
Brick, Target, Block : Number
'''
exec(compile_fargish(prog, saveto='testGloms.gen.py'), globals())

class TestGraph(TimeStepper, PortGraph):
    port_mates = port_mates

class TestGloms(unittest.TestCase):

    def setUp(self):
        stop_all_logging()

    def test_glom_2_nodes(self):
        g = TestGraph()
        b4 = g.make_node(Brick(4))
        b5 = g.make_node(Brick(5))
        b6 = g.make_node(Brick(6))
        g.do(GlomMerge([b4, b5]))
        self.assertEqual(len(g), 4)
        glomid = g.node_of_class(Glom)
        self.assertTrue(g.has_hop(glomid, 'glommees', b4, 'glom'))
        self.assertTrue(g.has_hop(glomid, 'glommees', b5, 'glom'))
        self.assertFalse(g.has_hop(glomid, 'glommees', b6, 'glom'))
