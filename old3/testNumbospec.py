# testNumbospec.py -- Unit tests for numbospec.py
#
# Temporarily obsolete. TODO Rewrite to solve a full numble using a brute-force
# model written in FARGish.

#import unittest
#
#from numbospec import *
#from PortGraph import PortGraph, pg, pn, pt
#from ExprAsEquation import ExprAsEquation
#from TimeStepper import TimeStepper
#from log import ShowActiveNodes, ShowActionList, ShowActionsChosen, \
#        ShowResults, stop_all_logging, log_all
#
#stop_all_logging()
#
#class TestGraph(TimeStepper, ExprAsEquation, PortGraph):
#    def __init__(self, numble, **kwargs):
#        super().__init__(**kwargs)
#        self.graph['numble'] = numble
#        self.ws = self.make_node(Workspace)
#        numble.build(self, self.ws)
#
#class TestNumbospec(unittest.TestCase):
#
#    def test_brick_is_target(self):
#        g = TestGraph(Numble([4, 5, 6, 15], 15))
#        self.assertFalse(g.done())
#        g.do_timestep()
#        self.assertTrue(g.done())
#        #TODO Assert success
#
#    def test_4_5_6_15(self):
#        #ShowActiveNodes.start_logging()
#        #ShowActionList.start_logging()
#        #ShowActionsChosen.start_logging()
#        #log_all()
#        g = TestGraph(Numble([4, 5, 6], 15), seed=4775339968402449257)
#        g.do_timestep(num=20)
#        ShowResults(g.done())
#        #pg(g)
#        self.assertTrue(g.done())
#
#if __name__ == '__main__':
#    log_all()
#    g = TestGraph(Numble([4, 5, 6], 15), seed=4775339968402449257)
#    g.do_timestep(num=20)
#    ShowResults(g.done())
#    #pg(g)
#    #self.assertTrue(g.done())
#    #TODO Assert success
