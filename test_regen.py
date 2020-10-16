# test_regen.py -- Tests of "regenerating" a group

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
    NoticeSameValue, AddAllInGlom, Brick, Plus, Number, Group
from regen import make_regenerator

class TestRegen(unittest.TestCase):

    def test_regen(self):
        pass


if __name__ == '__main__':
    g = newg()
    eqn = g.add_node(Group, member_of=g.ws)
    n11 = g.add_node(Number, 11, member_of=eqn)
    n4 = g.add_node(Number, 4, member_of=eqn)
    n7 = g.add_node(Number, 7, member_of=eqn)
    plus = g.add_node(Plus, operands=[n4, n7], result_consumer=n11)

    regenerator = make_regenerator(
        g, n11, place_in=g.ws, relevant_port_labels=[
            'consumer', 'operands', 'source', 'result_consumer'
        ]
    )
    pg(g, eqn, g.members_recursive(eqn), regenerator, g.members_recursive(regenerator))
