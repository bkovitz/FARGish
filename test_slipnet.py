# test_slipnet.py -- Tests of operations involving the slipnet

import unittest
from pprint import pprint as pp
import inspect

from log import *
from ActiveNode import make_action_sequence, ActionSeqNode, Start
from criteria import OfClass, IsAction
from ActiveGraph import pg

# HACK: We're drawing upon numbo5.py, which is ever-changing demo code.
# Properly, this test should only depend on stable production code and other
# test code.
from numbo5 import newg, SeekAndGlom, NoticeAllSameValue, CountMembers, \
    NoticeSameValue, AddAllInGlom, Brick

class TestCopyGroup(unittest.TestCase):

    def test_copy_group(self):
        # TODO Simplify this UT. It's way more complicated than it needs to be.
        g = newg()
        slipnet = g.slipnet
        ws = g.ws
        ns = g.find_all(OfClass(ActionSeqNode), within=slipnet)
        assert len(ns) == 1, 'numbo5 no longer has only one ActionSeqNode in the slipnet. Need to revise (unhack) this test.'
        old_action_seq_node = ns[0]
        #pg(g, g.members_recursive(old_action_seq_node) | {old_action_seq_node})

        # Copy the ActionSeqNode from the slipnet to the ws
        new_seq = g.copy_group(old_action_seq_node, ws)
        self.assertTrue(g.is_of_class(new_seq, ActionSeqNode))
        self.assertTrue(g.is_member(new_seq, ws))

        # Check that the members were copied, have the right attributes, and
        # are linked to each other correctly.
        new_members = g.members_recursive(new_seq)
        #pg(g, new_members | {new_seq.id})
        self.assertEqual(len(new_members), 5)

        #pg(g)
        for new_member in new_members:
            aft = g.activation_from_to(new_seq, new_member)
            self.assertEqual(aft, 0.5,
                f'Activation from {new_seq} to {new_member} is {aft}'
            )
#            self.assertTrue(g.has_hop(
#                new_seq, 'child_action', new_member, 'parent_action'
#            ), f'Missing child-parent link: {new_seq, new_member}')
            self.assertEqual(g.value_of(new_member, 'state'), Start)
        seek_and_glom = SeekAndGlom(OfClass(Brick), None)
        notice_all_same_value = NoticeAllSameValue(1, within=None)
        count_members = CountMembers(within=None)
        notice_same_value = NoticeSameValue(None, None)
        add_all_in_glom = AddAllInGlom(None, threshold=1.0)

        new_member1 = g.look_for(IsAction(seek_and_glom), within=ws)
        new_member2 = g.look_for(IsAction(notice_all_same_value), within=ws)
        new_member3 = g.look_for(IsAction(count_members), within=ws)
        new_member4 = g.look_for(IsAction(notice_same_value), within=ws)
        new_member5 = g.look_for(IsAction(add_all_in_glom), within=ws)
        self.assertTrue(new_member1)
        self.assertTrue(new_member2)
        self.assertTrue(new_member3)
        self.assertTrue(new_member4)
        self.assertTrue(new_member5)

        for prev, next in [
            (new_member1, new_member2),
            (new_member2, new_member3),
            (new_member3, new_member4),
            (new_member4, new_member5)
        ]:
            self.assertTrue(
                #g.has_hop(prev, 'next_action', next, 'prev_action'),
                g.has_hop(prev, 'next', next, 'prev'),
                f'Missing next-prev link: {prev, next}'
            )
            aft = g.activation_from_to(prev, next)
            self.assertEqual(aft, -2.0,
                f'Support from {prev} to {next} is {aft}.'
            )

        # TODO Verify that this feature works. Implementing it was hard!
        #self.assertEqual(g.min_activation(new_seq), 6.0)
