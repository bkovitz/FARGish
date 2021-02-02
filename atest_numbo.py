# atest_numbo.py -- Tests that Numbo can solve certain problems

import unittest
from itertools import chain

from NumboGraph import *
from log import *
from ActiveGraph import pg, pa
from PassiveChain import PassiveChain, make_passive_chain


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

class GraphWithPassiveChain(NumboGraph):

    def __init__(self, *args, seed=8028868705202140491, **kwargs):
        super().__init__(*args, seed=seed, **kwargs)
        self.max_actions = 2

    def make_initial_nodes(self):
        super().make_initial_nodes()
        target = self.look_for(Target, focal_point=self.ws)
        want = self.tag_of(target, Want)
        assessor = self.add_node(AssessProposal, behalf_of=want)
        self.set_support_from_to(want, assessor, 1.0)
        self.set_activation_from_to(want, assessor, 1.0)
        ncmp = self.add_node(NoticeCouldMakePlus, member_of=self.ws)
        ncmt = self.add_node(NoticeCouldMakeTimes, member_of=self.ws)
        #ncmm = self.add_node(NoticeCouldMakeMinus, member_of=self.ws)
        pdno = self.add_node(ProposeDoingNoticedOperation, member_of=self.ws)
        difft = self.add_node(DiffTagger, member_of=self.ws)
        diwt = None #self.add_node(DiffIsWantedTagger, member_of=self.ws)
        oot = self.add_node(OoMTagger, member_of=self.ws)
        oogtt = self.add_node(OoMGreaterThanTagger, member_of=self.ws)
        oo1bt = self.add_node(OoMSmallGapToWantedTagger, member_of=self.ws)
        oobigt = self.add_node(OoMBigGapToWantedTagger, member_of=self.ws) 
        nsolved = self.look_for(NoticeSolved)
        self.add_activation_autolinks(
            (OoMSmallGapToWanted, ncmp),
            #(OperandBelowWanted, ncmp),
            (OoMBigGapToWanted, ncmt),
            (OoMGreaterThan, [oo1bt, oobigt]),
            (OoM, [oogtt, oo1bt, oobigt]),
            (Diff, diwt),
            #(DiffIsWanted, ncmm),
            (Number, [difft, oot]),
            (Avail, nsolved),
            (Operator, pdno)  # TODO Only "noticed" Operators
        )

    def make_slipnet(self):
        sl = self.add_node(Slipnet)
        make_passive_chain(self,d
            Diff(value=1), DiffIsWanted, Minus, Proposal,
            member_of=sl
        )
        self.set_activation(self.members_recursive(sl), 0.0)

    def end_of_timestep(self):
        super().end_of_timestep()
        self.link_activation_to_archetypes(self.new_nodes)

    def allowable_active_nodes(self):
        return chain(
            super().allowable_active_nodes(), 
            self.find_all(PassiveChain, subset=self.members_of(self.slipnet))
        )

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

    def test_456_1(self):
        # The solution will need a Minus.
        g = GraphWithPassiveChain(Numble([4, 5, 6], 1))
        g.do_timestep(num=60)
        #print(f't={g.t}')
        self.assertTrue(g.succeeded())

if __name__ == '__main__':
    #g = GraphRunningActiveSeq(Numble([1, 1, 1, 1, 1], 5))
    #g = GraphWithPassiveChain(Numble([4, 5, 6], 30))
    g = GraphWithPassiveChain(Numble([4, 5, 6], 1))
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
