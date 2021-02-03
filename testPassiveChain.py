# testPassiveChain.py -- Unit tests for passive chains

import unittest
from pprint import pprint as pp
import inspect
from collections import Counter

from NumboGraph import *
from PassiveChain import PassiveChain, make_live_active_chain
from log import *
from ActiveGraph import pg, pa
from util import as_set

from criteria import Tagged as CTagged

class TestGraph(NumboGraph):

    def make_slipnet(self):
        sl = self.add_node(Slipnet)
        pc1 = self.add_node(PassiveChain, member_of=sl)
        n1 = self.add_node(Diff, value=1, member_of=pc1)
        n2 = self.add_node(DiffIsWanted, member_of=pc1)
        n3 = self.add_node(Minus, member_of=pc1)
        n4 = self.add_node(Proposal, member_of=pc1)
        self.link_sequence([n1, n2, n3, n4])
        self.set_mutual_activation(pc1, [n1, n2, n3, n4])
        self.set_activation(self.members_recursive(sl), 0.0)

class TestPassiveChain(unittest.TestCase):

    def test_need_basis_like(self):
        g = TestGraph(Numble([4, 5, 6], 15))
        b4, b5, b6 = g.get_nodes(Brick(4), Brick(5), Brick(6))
        diff = g.add_node(Diff(value=1), greater=b5, lesser=b4)
        [diw_archetype] = g.get_nodes(DiffIsWanted, within=g.slipnet)
        noticer = g.add_node(
            NoticeCouldMakeMinus, focal_point=diff, need_basis_like=diw_archetype
        )

        # noticer should sleep because it can't find a basis
        g.do_timestep(actor=noticer)
        self.assertTrue(g.is_sleeping(noticer))

        # now the noticer
        diw = g.add_node(DiffIsWanted, taggees=diff)
        g.do_timestep(actor=noticer)
        self.assertTrue(g.has_edge(noticer, 'basis', diw, 'basis_of'))
        self.assertFalse(g.is_sleeping(noticer))

    def test_run_passive_chain(self):
        results: List[Tuple(int, int)] = []
        num_trials = 10
        for _ in range(num_trials):
            results.append(self.run_one_passive_chain())

        # Now we check that the generated Proposal's operands were usually
        # the taggees of the Diff tag that triggered the PassiveChain.
        self.assertGreaterEqual(
            sum(1 for operands in results if operands == (5, 4)),
            0.6 * num_trials
        )

    def run_one_passive_chain(self) -> Tuple[int, int]:
        '''Builds a graph and runs a PassiveChain that should make a
        Proposal to try '5 - 4'. Returns (minuend_value, subtrahend_value)
        from the generated Proposal. Fails if no Proposal is generated.'''
        g = TestGraph(Numble([4, 5, 6], 1))
        b4, b5, b6 = g.get_nodes(Brick(4), Brick(5), Brick(6))

        # A Diff tag will trigger the PassiveChain
        diff1 = g.add_node(Diff(value=1), greater=b5, lesser=b4)
        passive_chain = g.get_node(PassiveChain, within=g.slipnet)
        diff1_archetype = g.initial_member_of(passive_chain)
        g.set_activation_from_to(diff1, diff1_archetype)

        # The PassiveChain should start a LiveActiveChain in the workspace
        g.do_timestep(actor=passive_chain)
        lac = g.neighbor(
            passive_chain, 'built', neighbor_class='LiveActiveChain'
        )
        self.assertTrue(lac, 'Failed to build LiveActiveChain')
        self.assertTrue(g.is_member(lac, g.ws))
        (diff_is_wanted_tagger, notice_could_make_minus,
         propose_doing_noticed_operation) = g.get_nodes(
         DiffIsWantedTagger, NoticeCouldMakeMinus,
         ProposeDoingNoticedOperation,
         within=lac
        )

        # Now let the system run. The active nodes inside the LiveActiveChain
        # should build a Minus and a Proposal to try it out.
        #ShowActionsPerformed.start_logging()
        g.do_timestep(num=15)
        proposal = g.look_for(Proposal, focal_point=g.ws)
        self.assertTrue(proposal, 'Failed to build Proposal')
        #pg(g)

        return (
            g.value_of(g.neighbor(proposal, 'proposed_minuend')),
            g.value_of(g.neighbor(proposal, 'proposed_subtrahend'))
        )

if __name__ == '__main__':
    #g = TestGraph(Numble([4, 5, 6], 15))
    g = TestGraph(Numble([4, 5, 6], 1), seed=1)
    b4, b5, b6 = g.get_nodes(Brick(4), Brick(5), Brick(6))
    diff = g.add_node(Diff(value=1), greater=b5, lesser=b4)
#    [diw_archetype] = g.get_nodes(DiffIsWanted, within=g.slipnet)
#    noticer = g.add_node(
#        NoticeCouldMakeMinus, focal_point=diff, need_basis_like=diw_archetype
#    )
    ShowActionList.start_logging()
    ShowActionsPerformed.start_logging()
#    g.do_timestep(actor=noticer)
#    diw = g.add_node(DiffIsWanted, taggees=diff)
#    g.do_timestep(actor=noticer)
#    pg(g, noticer)
    
    [pchain] = g.get_nodes(PassiveChain, within=g.slipnet)
    lac = make_live_active_chain(g, pchain, diff, member_of=g.ws)
    a1 = g.initial_member_of(lac)
    ShowPrimitives.start_logging()
    #g.do_timestep(actor=a1, num=2)
    pg(g, lac, a1)
    g.do_timestep(num=9)
