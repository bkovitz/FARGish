import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Callable
from operator import add, mul
from functools import reduce

from Ac import Ac, AcNode, AdHocAcNode, All, AllAre, TagWith, AddNode, OrFail, \
    MembersOf, Len, EqualValue, Taggees, LookFor, Raise, PrintEnv
from codegen import make_python, compile_fargish
from criteria import OfClass, Tagged as CTagged, HasThisValue
from StdGraph import Graph, pg 
from Numble import make_numble_class
from testNodeClasses import *
from Node import Node, NRef, NRefs, CRef, MaybeNRef
from ActiveNode import ActiveNode, Start, Completed
from Action import Action, Actions, SelfDestruct
from log import *
from util import first
from exc import AcNeedArg, ActionFailure, AcFailed, FargDone

prog = '''
tags -- taggees
within -- overriding
node1 -- overriding
node2 -- overriding
target -- overriding
consume_operands -- proposer
proposed_operator -- proposer
result_consumer -- source  # HACK: should be 'consumer'; see unique_mate().

Workspace

Tag(taggees)
AllMembersHaveThisValue : Tag
SameValue, Consumed, Done : Tag
Blocked(reason) : Tag
Failed(reason): Tag
Count(value) : Tag


Group(members)
Glom : Group
'''
exec(compile_fargish(prog), globals())

Numble = make_numble_class(
    Brick, Target, Want, Avail, Allowed, [Plus, Times]
)

@dataclass
class NumboSuccess(FargDone):
    node: NRef
    target: NRef

class TestGraph(Graph):

    def __init__(self, numble, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.nodeclasses.update(nodeclasses)
        self.port_mates += port_mates
        ws = self.add_node(Workspace)
        numble.build(self, ws)

    def consume_operands(
        self,
        operand_ids: NRefs,
        operator_class: CRef,
        actor=None
    ):
        if not self.has_tag(operand_ids, Avail):
            return  # TODO Raise a failure exception?
        operator_class = self.as_nodeclass(operator_class)
        operator_id = self.add_node(
            operator_class, 
            operands=operand_ids,
        )
        result_id = self.add_node(
            Block,
            value=arith_result0(self, operator_class, operand_ids),
            source=operator_id,
        )
        self.move_tag(Avail, operand_ids, result_id)
        self.add_tag(Consumed, operand_ids)
        self.add_tag(Done, actor)

def arith_result0(g, operator_class, operand_ids):
    operand_values = [g.value_of(o) for o in operand_ids]
    # TODO It would be much better if FARGish let you define these operations
    # as class attributes.
    if operator_class is None:
        return None
    elif operator_class == Plus:
        return reduce(add, operand_values, 0)
    elif operator_class == Times:
        return reduce(mul, operand_values, 1)
    else:
        #raise ValueError(f'Unknown operator class {operator_class} of node {operator_id}.')
        raise ValueError(f'Unknown operator class {operator_class}.')

class AllBricksAvail(Tag, ActiveNode):

    def actions(self):
        pass

    def update(self) -> Actions:
        bricks = self.g.find_all(OfClass(Brick))
        if not AllTagged(self.g, Avail, bricks):
            return [SelfDestruct(self)]
        

class Proposal(ActiveNode):
    node_params = NodeParams(AttrParam('action'))
    # We expect more arguments, which we will pass to 'action'.

    is_duplicable = True  # HACK  Is already_built mis-rejecting this?

    def actions(self):
        return self.action.with_overrides_from(self.g, self)

@dataclass
class ConsumeOperands(Action):
    consume_operands: Union[NRefs, None]=None
    proposed_operator: Union[NRef, None]=None

    def go(self, g, actor):
        g.consume_operands(self.consume_operands, self.proposed_operator)
        g.new_state(self.actor, Completed)

@dataclass
class NotAllThisValue(AcFailed):
    #value: Any=None
    #within: NRef=None
    ac: Ac
    actor: MaybeNRef

class TestAc(unittest.TestCase):

    def setUp(self):
        stop_all_logging()

    def test_do_notice_and_tag(self):
        # Tests running Ac objects directly. Normally, though, you only run
        # Ac objects from inside an AcNode.
        g = TestGraph(Numble([4, 5, 6], 15))
        targetid = 1  # HACK

        env = Ac.run(
            g,
            All(OfClass(Brick), within=g.ws),
            actor=targetid
        )
        nodes = map(g.as_node, env['nodes'])
        self.assertCountEqual(nodes, [Brick(4), Brick(5), Brick(6)])

        # Should fail because All is missing a 'within' arg.
        try:
            env = Ac.run(g, All(OfClass(Brick)), actor=targetid)
        except AcNeedArg as exc:
            self.assertEqual(
                exc, AcNeedArg(ac=All(OfClass(Brick)), name='within')
            )
        else:
            self.fail("Missing 'within' failed to raise AcNeedArg.")

        env = Ac.run(g, [
            All(OfClass(Brick), within=g.ws),
            AllAre(CTagged(Avail))
        ], actor=targetid)
        nodes = map(g.as_node, env['nodes'])
        self.assertCountEqual(nodes, [Brick(4), Brick(5), Brick(6)])

        env = Ac.run(g, [
            All(OfClass(Brick), within=g.ws),
            AllAre(CTagged(Avail)),
            TagWith(AllBricksAvail, taggees='nodes')
        ], actor=targetid)
        result = list(g.as_nodes(env['result']))

        self.assertEqual(len(result), 1)
        tag = result[0]
        self.assertEqual(tag.__class__, AllBricksAvail)
        self.assertTrue(g.has_tag(nodes, tag))

    def test_noticer(self):
        g = TestGraph(Numble([4, 5, 6], 15))
        
        noticer = g.add_node(AdHocAcNode, [
            All(OfClass(Brick), within=g.ws),
            AllAre(CTagged(Avail)),
            TagWith(AllBricksAvail, taggees='nodes')
        ], member_of=g.ws)

        #pg(g)
        #ShowActiveNodes.start_logging()
        #ShowActiveNodesCollected.start_logging()
        #ShowActionList.start_logging()
        #ShowActionsChosen.start_logging()

        #g.do_timestep()
        #print('STATE', noticer.state, noticer.can_go())
        self.assertEqual(noticer.state, Start)
        self.assertTrue(noticer.can_go())
        self.assertIn(noticer.id, g.as_nodeids(g.active_nodes()))
        g.do_timestep(actor=noticer)
        tag = g.as_node(first(g.new_nodes))
        self.assertEqual(tag.__class__, AllBricksAvail)
        self.assertEqual(noticer.state, Completed)
        self.assertFalse(noticer.can_go())
        self.assertNotIn(noticer.id, g.as_nodeids(g.active_nodes()))

    def test_override(self):
        class Noticer(AcNode):
            acs = [
                All(OfClass(Brick)),
                AllAre(CTagged(Avail)),
                TagWith(AllBricksAvail, taggees='nodes')
            ]

        g = TestGraph(Numble([4, 5, 6], 15))
        glom = g.add_node(Glom, g.find_all(OfClass(Brick)))

        noticer = g.add_node(Noticer, member_of=g.ws)

        self.assertIn(noticer.id, g.as_nodeids(g.active_nodes()))
        g.do_timestep(actor=noticer)
        self.assertTrue(g.has_tag(noticer, Blocked))

        self.assertNotIn(noticer.id, g.as_nodeids(g.active_nodes()))

        g.add_override_node(noticer, 'within', glom)
        g.remove_tag(noticer, Blocked)

        g.do_timestep(actor=noticer)
        bricks = g.find_all(OfClass(Brick))
        self.assertTrue(
            g.has_tag(bricks, AllBricksAvail),
            "Did not tag the Bricks even when 'within' was overridden."
        )

    def test_ac_add_node(self):
        class SeekAndGlom(AcNode):
            acs = [
                All(OfClass(Brick)),
                AddNode(Glom, members='nodes')
            ]

        g = TestGraph(Numble([4, 5, 6], 15))
        bricks = g.find_all(OfClass(Brick))

        seek_and_glom = g.add_node(SeekAndGlom, member_of=g.ws, within=g.ws)

        g.do_timestep(actor=seek_and_glom)
        glom = g.look_for(
            OfClass(Glom), subset=g.neighbors(bricks, 'member_of')
        )

        self.assertTrue(glom, 'Did not build Glom')
        self.assertCountEqual(g.neighbors(glom, 'members'), bricks)

    class NoticeAllHaveThisValue(AcNode):
        acs = [
            All(OfClass(Number)),
            OrFail(
                AllAre(HasThisValue(value=3)),
                NotAllThisValue
            ),
            TagWith(AllMembersHaveThisValue, taggees='within')
        ]

    # TODO test overriding 'value' with the value of another node
    # TODO test that noticer Fails if bricks don't all have the value
    def test_ac_has_value(self):
        g = TestGraph(Numble([3, 3, 3], 15))
        glom = g.add_node(Glom, g.find_all(OfClass(Brick)))

        noticer = g.add_node(
            self.NoticeAllHaveThisValue, member_of=g.ws, within=glom
        )

        g.do_timestep(actor=noticer)
        self.assertTrue(g.has_tag(glom, AllMembersHaveThisValue))

    def test_ac_has_value_fail(self):
        g = TestGraph(Numble([4, 5, 6], 15))
        glom = g.add_node(Glom, g.find_all(OfClass(Brick)))

        noticer = g.add_node(
            self.NoticeAllHaveThisValue, member_of=g.ws, within=glom
        )

        g.do_timestep(actor=noticer)
        self.assertFalse(g.has_tag(glom, AllMembersHaveThisValue))
        self.assertTrue(g.has_tag(noticer, Failed))

    def test_ac_count_members(self):
        class CountMembers(AcNode):
            acs = [
                MembersOf('within'),
                Len('nodes'),
                TagWith(Count, taggees='within', value='value')
            ]

        g = TestGraph(Numble([4, 5, 6], 15))
        glom = g.add_node(Glom, g.find_all(OfClass(Brick)))

        self.assertFalse(g.has_tag(glom, Count(value=3)))

        counter = g.add_node(CountMembers, within=glom)
        g.do_timestep(actor=counter)
        self.assertTrue(g.has_tag(glom, Count(value=3)))

    def test_ac_notice_same_value(self):
        class NoticeSameValue(AcNode):
            acs = [
                EqualValue('node1', 'node2'),
                Taggees('node1', 'node2'),
                TagWith(SameValue)
            ]

        g = TestGraph(Numble([1, 1, 1], 3))
        target = g.look_for(OfClass(Target))
        glom = g.add_node(Glom, g.find_all(OfClass(Brick)))
        count = g.add_node(Count, taggees=glom, value=3)

        noticer = g.add_node(
            NoticeSameValue,
            node1=target,
            node2=count
        )
        g.do_timestep(actor=noticer)
        self.assertTrue(g.has_tag([count, target], SameValue))

    def test_ac_add_all_in_glom(self):
        class AddAllInGlom(AcNode):
            acs = [
                All(OfClass(Number), CTagged(Avail)),
                LookFor(OfClass(Plus), CTagged(Allowed)),
                AddNode(
                    Proposal,
                    action=ConsumeOperands(),
                    consume_operands='nodes',
                    proposed_operator='node',
                )
            ]

        g = TestGraph(Numble([4, 5, 6], 15))
        glom = g.add_node(Glom, g.find_all(OfClass(Brick)))
        proposer = g.add_node(AddAllInGlom, within=g.ws)

        g.do_timestep(actor=proposer)

        proposal = g.neighbor(proposer, 'built')
        self.assertEqual(g.class_of(proposal), Proposal)

        g.do_timestep(actor=proposal)

        self.assertTrue(
            g.has_tag(proposal, Block(15), taggee_port_label='built')
        )

    def test_ac_notice_solved(self):
        class NoticeSolved(AcNode):
            acs = [
                LookFor(CTagged(Avail), cond=EqualValue('node', 'target')),
                Raise(NumboSuccess, node='node', target='target')
            ]

        g = TestGraph(Numble([4, 5, 6, 15], 15))
        target = g.look_for(OfClass(Target))
        glom = g.add_node(Glom, g.find_all(OfClass(Brick)))

        noticer = g.add_node(NoticeSolved, within=glom, target=target)

        g.do_timestep(actor=noticer)

        got = g.done()
        self.assertEqual(got.__class__, NumboSuccess)
        self.assertEqual(g.as_node(got.node), Brick(15))
        self.assertEqual(g.as_node(got.target), Target(15))

    def test_ac_selfdestruct_on_update(self):
        g = TestGraph(Numble([4, 5, 6], 15))
        bricks = g.find_all(OfClass(Brick))
        tag = g.add_tag(AllBricksAvail, bricks)

        self.assertCountEqual(as_iter(g.actions(tag)), [])

        g.remove_tag(bricks[0], Avail)  # now all Bricks are no longer Avail
        g.do_timestep(actor=tag)

        self.assertFalse(
            g.has_node(tag),
            'AllBricksAvail did not SelfDestruct.'
        )

        # Trying to run the tag after it no longer exists should not cause
        # an exception.
        g.do_timestep(actor=tag)
