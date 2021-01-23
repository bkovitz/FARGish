# testPassiveChain.py -- Unit tests for passive chains

import unittest
from pprint import pprint as pp
import inspect
from collections import Counter

from NumboGraph import *
from PassiveChain import PassiveChain, RunPassiveChain, PassiveChainRunner
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

    def test_run_passive_chain(self):
        test_counter = Counter()
        # counts ((greater, lesser), (minuend, subtrahend)) in order to verify
        # that usually the Minus's operands match the Diff's taggees

        for testi in range(1):
            g = TestGraph(Numble([4, 5, 6], 1))
            if testi == 0:
                pg(g, Brick)
            difft = g.add_node(DiffTagger, member_of=g.ws)

            print('T0')
            g.do_timestep(actor=difft, num=6)
            print('T1') #DEBUG
            diff1 = g.look_for(Diff(value=1), focal_point=g.ws)
            assert diff1, "Initialization didn't build a Diff(value=1) tag."
            passive_chain = g.look_for(PassiveChain, focal_point=g.slipnet)
            assert passive_chain, "Initialization didn't find a PassiveChain in the slipnet."
            diff1_archetype = g.initial_member_of(passive_chain)
            assert diff1_archetype, "Passive chain lacks its initial member"
            g.set_activation_from_to(diff1, diff1_archetype)
            #ShowPrimitives.start_logging()
            #ShowActionsPerformed.start_logging()

            # This should start a PassiveChainRunner
            g.do_timestep(actor=passive_chain)
            runner = g.look_for(PassiveChainRunner, subset=g.new_nodes)
            self.assertTrue(runner,
                'PassiveChain failed to build PassiveChainRunner')

            g.do_timestep(actor=runner)
            agent1 = g.look_for(DiffIsWantedTagger, subset=g.new_nodes)
            self.assertTrue(agent1)

            g.do_timestep(actor=agent1)
            tag1 = g.look_for(DiffIsWanted, subset=g.new_nodes)
            self.assertTrue(tag1)

            g.do_timestep(actor=runner)
            agent2 = g.look_for(NoticeCouldMakeMinus, subset=g.new_nodes)
            self.assertTrue(agent2)

            pg(g, Brick) #DEBUG
            for _ in range(6):
                g.do_timestep(actor=as_set(g.walk(agent2, 'agents')))
                minus = g.neighbor(agent2, 'completion', neighbor_class=Minus)
                if minus:
                    break
            else:
                self.fail('NoticeCouldMakeMinus never built a Minus')
            #pg(g, NoticeCouldMakeMinus, Minus) #DEBUG

            # Verify that the Minus's operands are the taggees of the right Diff(1),
            # viz. the one that triggered the PassiveChainRunner.
            '''
            self.assertEqual(
                g.neighbor(diff1, 'greater'),
                g.neighbor(minus, 'minuend')
            )
            self.assertEqual(
                g.neighbor(diff1, 'lesser'),
                g.neighbor(minus, 'subtrahend')
            )
            '''

            g.do_timestep(actor=runner)
            agent3 = g.look_for(ProposeDoingNoticedOperation, subset=g.new_nodes)
            self.assertTrue(agent3)

            g.do_timestep(actor=agent3)
            proposal = g.look_for(Proposal, subset=g.new_nodes)
            self.assertTrue(proposal)

            g.do_timestep(actor=runner)
            self.assertEqual(g.getattr(runner, 'state'), Completed)

            got_tup = (
                (g.neighbor(diff1, 'greater'), g.neighbor(diff1, 'lesser')),
                (g.neighbor(minus, 'minuend'), g.neighbor(minus, 'subtrahend'))
            )
            test_counter[got_tup] += 1

            #target = g.look_for(Target, focal_point=g.ws)
            #pg(g, runner, agent3, minus, Diff)
            #print('UT', g.new_nodes, )

        same_operands_counter = Counter()
        for tup, count in test_counter.items():
            if tup[0] == tup[1]:
                same_operands_counter[True] += count
            else:
                same_operands_counter[False] += count
        print('UT', test_counter.items())
        print(same_operands_counter)
