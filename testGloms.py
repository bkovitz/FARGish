# testGloms.py -- Unit test to verify that gloms work

import unittest

#from PortGraph import PortGraph, pg, ps
from StdGraph import Graph, pg
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

class TestGraph(Graph):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.port_mates += port_mates

class TestGloms(unittest.TestCase):

    def setUp(self):
        stop_all_logging()

    def test_glom_basics(self):
        g = TestGraph()
        b4 = g.add_node(Brick(4))
        b5 = g.add_node(Brick(5))
        b6 = g.add_node(Brick(6))
        g.do(GlomMerge([b4, b5]))
        self.assertEqual(len(g), 4)
        glomid = g.node_of_class(Glom)
        self.assertTrue(g.has_hop(glomid, 'glommees', b4, 'glom'))
        self.assertTrue(g.has_hop(glomid, 'glommees', b5, 'glom'))
        self.assertFalse(g.has_hop(glomid, 'glommees', b6, 'glom'))

        #Now, extend the glom
        g.do(GlomMerge([glomid, b6]))
        self.assertEqual(len(g), 4)
        self.assertTrue(g.has_hop(glomid, 'glommees', b6, 'glom'))

    def test_from_one_glom_to_another(self):
        g = TestGraph()
        b4 = g.add_node(Brick(4))
        b5 = g.add_node(Brick(5))
        b6 = g.add_node(Brick(6))
        b7 = g.add_node(Brick(7))
        g.do(GlomMerge([b4, b5]))
        g.do(GlomMerge([b6, b7]))
        gl1 = g.neighbor(b4, 'glom')
        gl2 = g.neighbor(b6, 'glom')
        g.do(GlomMerge([b4, gl2]))
        self.assertCountEqual(g.neighbors(b4, port_label='glom'), [gl2])
