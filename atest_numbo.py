# atest_numbo.py -- Tests that Numbo can solve certain problems

import unittest

from NumboGraph import *
from log import *
from ActiveGraph import pg, pa


class GraphRunningActiveSeq(NumboGraph):

    def __init__(self, *args, seed=8028868705202140491, **kwargs):
        super().__init__(*args, seed=seed, **kwargs)

    def make_initial_nodes(self):
        super().make_initial_nodes()
        # HACK self.seqnode is the ActionSeqNode holding several ActionNodes
        # that should (clumsily) solve this numble.
        target = self.look_for(Target, focal_point=self.ws)
        want = self.tag_of(target, Want)
        assessor = self.add_node(AssessProposal, behalf_of=want)
        self.set_support_from_to(want, assessor, 1.0)

        neural_program = self.copy_group(self.seqnode, self.ws)
        neural_program.min_activation = 10.0
        neural_program.min_support_for = 10.0

class NumboTest(unittest.TestCase):

    def setUp(self):
        stop_all_logging()

    def test_11111_5(self):
        g = GraphRunningActiveSeq(Numble([1, 1, 1, 1, 1], 5))

        g.do_timestep(num=60)
        #print('DONE', repr(g.done()))
        self.assertTrue(g.succeeded())
        #print(f't={g.t}')
        # TODO Test that the equation is 1 + 1 + 1 + 1 + 1 = 5

    def test_22222_10(self):
        g = GraphRunningActiveSeq(Numble([2, 2, 2, 2, 2], 10))

        g.do_timestep(num=60)
        self.assertTrue(g.succeeded())
        #print(f't={g.t}')
        # TODO Test for expected equation

if __name__ == '__main__':
    g = GraphRunningActiveSeq(Numble([1, 1, 1, 1, 1], 5))
    #g.max_actions = 2
    ShowActiveNodes.start_logging()
    ShowActionList.start_logging()
    ShowActionsPerformed.start_logging()
    ShowPrimitives.start_logging()
    #neural_program = g.copy_group(g.seqnode, g.ws)
    #neural_program.min_activation = 10.0
    #neural_program.min_support_for = 10.0
    g.do_timestep(num=31)
    #pg(g)
    #print(g.done())
    #g.do_timestep()
