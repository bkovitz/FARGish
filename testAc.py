# testAc.py -- Unit tests for Ac objects and ActiveNodes made from them

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable
from math import log10

from Ac import Ac, AcNode, AdHocAcNode, All, AllAre, TagWith, AddNode, OrFail, \
    MembersOf, Len, EqualValue, Taggees, LookFor, Raise, PrintEnv, AcNot, \
    SelfDestruct, FindParamName, LookForArg, AddOverride, RemoveBlockedTag, \
    LookForTup
from codegen import make_python, compile_fargish
from criteria import OfClass, Tagged as CTagged, HasThisValue, NotTheArgsOf
from StdGraph import Graph, MyContext, InWorkspace, pg
from NumboGraph import *
from Node import Node, NRef, NRefs, CRef, MaybeNRef, as_nodeid, as_nodeids
from ActiveNode import ActiveNode, Start, Completed, HasUpdate, Sleeping
from Action import Action, Actions, BuildAgent, BoostFromTo
from log import *
from util import first
from exc import FargDone, NeedArg

@dataclass
class FoundNode(FargDone):
    node: NRef

@dataclass
class FoundTup(FargDone):
    tup: Tuple[NodeId]

class TestAc(unittest.TestCase):

    def setUp(self):
        stop_all_logging()

    def test_do_notice_and_tag(self):
        # Tests running Ac objects directly. Normally, though, you only run
        # Ac objects from inside an AcNode.
        g = NumboGraph(Numble([4, 5, 6], 15))
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
        except NeedArg as exc:
            self.assertEqual(
                exc, NeedArg(ac=All(OfClass(Brick)), name='within')
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
        g = NumboGraph(Numble([4, 5, 6], 15))
        
        noticer = g.add_node(AdHocAcNode, [
            All(OfClass(Brick), within=g.ws),
            AllAre(CTagged(Avail)),
            TagWith(AllBricksAvail, taggees='nodes')
        ], member_of=g.ws)

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
        g = NumboGraph(Numble([4, 5, 6], 15))
        glom = g.add_node(Glom, g.find_all(OfClass(Brick)))

        noticer = g.add_node(NoticeAllBricksAreAvail, member_of=g.ws)
        self.assertIn(noticer.id, g.as_nodeids(g.active_nodes()))

        g.do_timestep(actor=noticer)

        # Since the NoticeAllBricksAreAvail can't run, it should get tagged
        # Blocked(NeedArg('within')).
        tag = g.as_node(g.tag_of(noticer, Blocked))
        self.assertTrue(
            tag,
            "Failed to create Blocked tag for missing 'within' argument."
        )
        reason = g.getattr(tag, 'reason')
        self.assertTrue(isinstance(reason, NeedArg))
        self.assertEqual(reason.name, 'within')

        self.assertTrue(g.is_blocked(noticer))

        # Now we manually override the 'within' argument.
        g.add_override_node(noticer, 'within', glom)
        g.remove_tag(noticer, Blocked)

        g.do_timestep(actor=noticer)
        bricks = g.find_all(OfClass(Brick))
        self.assertTrue(
            g.has_tag(bricks, AllBricksAvail),
            "Did not tag the Bricks even when 'within' was overridden."
        )

    def test_ac_add_node(self):
        g = NumboGraph(Numble([4, 5, 6], 15))
        bricks = g.find_all(OfClass(Brick))

        seek_and_glom = g.add_node(SeekAndGlom, member_of=g.ws, within=g.ws)

        g.do_timestep(actor=seek_and_glom)
        glom = g.look_for(
            OfClass(Glom), subset=g.neighbors(seek_and_glom, 'built')
        )

        self.assertTrue(glom, 'Did not build Glom')
        self.assertCountEqual(g.neighbors(glom, 'members'), bricks)

    # TODO test overriding 'value' with the value of another node
    def test_ac_has_value(self):
        g = NumboGraph(Numble([3, 3, 3], 15))
        glom = g.add_node(Glom, g.find_all(OfClass(Brick)))

        noticer = g.add_node(
            NoticeAllHaveThisValue, member_of=g.ws, within=glom
        )

        g.do_timestep(actor=noticer)
        self.assertTrue(g.has_tag(glom, AllMembersHaveThisValue))

    def test_ac_has_value_fail(self):
        g = NumboGraph(Numble([4, 5, 6], 15))
        glom = g.add_node(Glom, g.find_all(OfClass(Brick)))

        noticer = g.add_node(
            NoticeAllHaveThisValue, member_of=g.ws, within=glom
        )

        g.do_timestep(actor=noticer)
        self.assertFalse(g.has_tag(glom, AllMembersHaveThisValue))
        self.assertTrue(g.has_tag(noticer, Failed))

    def test_ac_specify_value_in_init(self):
        # Tests passing arguments lacking a NodeParam to the Node ctor rather
        # than by going through g.add_node(). The custom arguments should
        # appear in the Node object.
        noticer = NoticeAllHaveThisValue(value=5, whatever=0.123)
        self.assertEqual(noticer.value, 5)
        self.assertEqual(noticer.whatever, 0.123)

    def test_ac_shadow_criterion_fields(self):
        # Tests that fields in Criterion objects can be overridden by the
        # same mechanisms as fields in Ac objects.
        g = NumboGraph(Numble([4, 5, 6], 15))
        target = g.look_for(OfClass(Target), within=g.ws)
        assert target, 'No Target node'
        seeker = g.add_node(SeekAndGlom(seekclass=Target, within=g.ws))

        g.do_timestep(actor=seeker)
        glom = g.look_for(
            OfClass(Glom), subset=g.neighbors(seeker, 'built')
        )

        self.assertTrue(glom, 'Did not build Glom')
        self.assertCountEqual(g.neighbors(glom, 'members'), [target])

    def test_ac_count_members(self):
        class CountMembers(AcNode):
            acs = [
                MembersOf('within'),
                Len('nodes'),
                TagWith(Count, taggees='within', value='value')
            ]

        g = NumboGraph(Numble([4, 5, 6], 15))
        glom = g.add_node(Glom, g.find_all(OfClass(Brick)))

        self.assertFalse(g.has_tag(glom, Count(value=3)))

        counter = g.add_node(CountMembers, within=glom)
        g.do_timestep(actor=counter)
        self.assertTrue(g.has_tag(glom, Count(value=3)))

    def test_ac_inworkspace(self):
        class FindPlus(AcNode):
            acs = [
                LookFor(OfClass(Plus), within=InWorkspace),
                Raise(FoundNode, node='node')
            ]

        g = NumboGraph(Numble([4, 5, 6], 15))
        plus = g.look_for(OfClass(Plus))
        assert plus, 'No Plus in workspace'

        finder = g.add_node(FindPlus)
        self.assertEqual(
            as_nodeid(InWorkspace.within(g, finder)),
            as_nodeid(g.ws)
        )

        g.do_timestep(actor=finder)

        self.assertEqual(g.done(), FoundNode(plus))

    def test_ac_mycontext(self):
        class FindPlus(AcNode):
            acs = [
                LookFor(Plus, within=MyContext),
                Raise(FoundNode, node='node')
            ]
        
        g = NumboGraph(Numble([4, 5, 6], 15))
        wrong_plus = g.look_for(OfClass(Plus), within=g.ws)
        assert wrong_plus, 'No Plus in workspace'
        glom = g.add_node(Glom)
        right_plus = g.add_node(Plus, member_of=glom)

        finder = g.add_node(FindPlus, member_of=glom)
        self.assertEqual(
            as_nodeid(MyContext.within(g, finder)),
            as_nodeid(glom.id)
        )

        g.do_timestep(actor=finder)

        self.assertEqual(g.done(), FoundNode(right_plus.id))

    def test_ac_notice_same_value(self):
        g = NumboGraph(Numble([1, 1, 1], 3))
        target = g.look_for(OfClass(Target))
        glom = g.add_node(Glom, g.find_all(OfClass(Brick)))
        count = g.add_node(Count, taggees=glom, value=3)

        noticer = g.add_node(
            NoticeCountSameAsTarget,
            node1=target,
            node2=count,
            within=g.ws
        )
        g.do_timestep(actor=noticer)
        self.assertTrue(g.has_tag([count, target], SameValue))
        self.assertFalse(g.is_active(noticer))

    def test_ac_notice_same_value_with_search(self):
        g = NumboGraph(Numble([1, 1, 1], 3))
        target = g.look_for(OfClass(Target))
        glom = g.add_node(Glom, g.find_all(OfClass(Brick)))
        count = g.add_node(Count, taggees=glom, value=3)

        noticer = g.add_node(
            NoticeCountSameAsTarget,
            within=InWorkspace
        )
        g.do_timestep(actor=noticer)
        self.assertTrue(g.has_tag([count, target], SameValue))

    def test_ac_notice_same_value_fail_eq(self):
        g = NumboGraph(Numble([1, 1, 1], 4))
        target = g.look_for(OfClass(Target))
        glom = g.add_node(Glom, g.find_all(OfClass(Brick)))
        count = g.add_node(Count, taggees=glom, value=3)

        noticer = g.add_node(
            NoticeCountSameAsTarget,
            within=InWorkspace
        )
        g.do_timestep(actor=noticer)
        self.assertTrue(g.is_failed(noticer))
        # TODO Check that Failed tag has correct reason

    def test_ac_add_all_in_glom(self):
        g = NumboGraph(Numble([4, 5, 6], 15))
        glom = g.add_node(Glom, g.find_all(OfClass(Brick)))
        proposer = g.add_node(AddAllInGlom, within=glom)

        a_brick = g.look_for(OfClass(Brick), within=g.ws)
        self.assertEqual(g.containers_of_recursive(a_brick), {glom.id, g.ws.id})

        g.do_timestep(actor=proposer)

        proposal = g.as_node(g.neighbor(proposer, 'built'))
        self.assertEqual(g.class_of(proposal), Proposal)

        g.do_timestep(actor=proposal)

        new_plus = g.neighbor(proposal, 'built', neighbor_class=Plus)
        self.assertTrue(new_plus, 'Proposal did not build Plus.')
        self.assertTrue(g.is_member(new_plus, g.ws))
        self.assertFalse(g.is_member(new_plus, glom))
        self.assertTrue(g.is_dormant(proposal))

        block = g.as_node(g.neighbor(proposal, 'built', neighbor_class=Block))
        self.assertEqual(g.as_node(block), Block(15))
        self.assertTrue(g.is_member(block, g.ws))
        self.assertFalse(g.is_member(block, glom))

    def test_ac_notice_solved(self):
        class NoticeSolved(AcNode):
            acs = [
                LookFor(CTagged(Avail), cond=EqualValue('node', 'target')),
                Raise(NumboSuccess, node='node', target='target')
            ]

        g = NumboGraph(Numble([4, 5, 6, 15], 15))
        target = g.look_for(OfClass(Target))
        glom = g.add_node(Glom, g.find_all(OfClass(Brick)))

        noticer = g.add_node(NoticeSolved, within=glom, target=target)

        g.do_timestep(actor=noticer)

        got = g.done()
        self.assertEqual(got.__class__, NumboSuccess)
        self.assertEqual(g.as_node(got.node), Brick(15))
        self.assertEqual(g.as_node(got.target), Target(15))
        self.assertTrue(g.succeeded())

    def test_ac_lookfor_not_there(self):
        # Tests that LookFor does not crash if no nodes meet the criterion.
        class Looker(AcNode):
            acs = [
                LookFor(OfClass(Count)),
                Raise(FoundNode, node='node')
            ]

        g = NumboGraph(Numble([4, 5, 6, 15], 15))
        looker = g.add_node(Looker, within=g.ws)

        g.do_timestep(actor=looker)
        self.assertFalse(g.done())

    def test_ac_lookfortup(self):
        class Looker(AcNode):
            acs = [
                LookForTup(
                    [CTagged(Avail), CTagged(Avail)],
                    tupcond=NotTheArgsOf(Plus, 'operands'),
                    within=InWorkspace
                ),
                Raise(FoundTup, tup='nodes')
            ]
        g = NumboGraph(Numble([4, 5, 6], 15))
        b4 = g.look_for(Brick(4))
        b5 = g.look_for(Brick(5))
        b6 = g.look_for(Brick(6))
        plus = g.add_node(Plus, operands=[b4, b5])
        looker = g.add_node(Looker, within=g.ws)

        g.do_timestep(actor=looker)

        expect = [
            (b4, b6), (b5, b6),  # brick pairs that are not the operands
            (b6, b4), (b6, b5)   # of plus.
        ]
        done = g.done()
        self.assertIsInstance(done, FoundTup)
        self.assertIn(done.tup, expect)
        
    def test_ac_selfdestruct_on_update(self):
        g = NumboGraph(Numble([4, 5, 6], 15))
        bricks = g.find_all(OfClass(Brick))
        tag = g.add_tag(AllBricksAvail, bricks)

        self.assertFalse(tag.needs_update)
        self.assertCountEqual(as_iter(g.actions(tag)), [])

        g.remove_tag(bricks[0], Avail)  # now all Bricks are no longer Avail

        g.do_touches()  # UGLY: calling .do_timestep() clears the touch made
                        # by .remove_tag(), so we force .do_touches() here.

        self.assertTrue(tag.needs_update)
        g.do_timestep(actor=tag)

        self.assertFalse(
            g.has_node(tag),
            'AllBricksAvail did not SelfDestruct.'
        )

        # Trying to run the tag after it no longer exists should not cause
        # an exception.
        g.do_timestep(actor=tag)

    def test_ac_fillparamscout(self):
        g = NumboGraph(Numble([4, 5, 6], 15))
        glom = g.add_node(Glom, g.find_all(OfClass(Brick)))
        noticer = g.add_node(NoticeAllBricksAreAvail, member_of=g.ws)
        tag = g.add_tag(
            Blocked(reason=NeedArg(ac=noticer.action, name='within')),
            noticer
        )
        scout = g.add_node(FillParamScout, behalf_of=noticer, problem=tag)
        g.do_timestep(actor=scout)

        # The FillParamScout should do the override:
        self.assertTrue(g.has_hop(noticer, 'within', glom, 'overriding'))

        # and remove the Blocked tag:
        self.assertFalse(g.has_node(tag))

    def test_ac_already_built_fillparamscout(self):
        g = NumboGraph(Numble([4, 5, 6], 15))
        noticer = g.add_node(NoticeAllBricksAreAvail, member_of=g.ws)
        tag = g.add_tag(
            Blocked(reason=NeedArg(ac=noticer.action, name='within')),
            noticer
        )
        scout = g.add_node(FillParamScout, behalf_of=noticer, problem=tag)

        self.assertTrue(g.already_built(
            FillParamScout, behalf_of=noticer, problem=tag
        ))

    def test_ac_already_built_acnode_without_args(self):
        class MyScout(AcNode):
            # There are no NodeParams here. This exposed a bug once.
            acs = [
                FindParamName()
            ]

        g = NumboGraph(Numble([4, 5, 6], 15))
        noticer = g.add_node(NoticeAllBricksAreAvail, member_of=g.ws)
        tag = g.add_tag(
            Blocked(reason=NeedArg(ac=noticer.action, name='within')),
            noticer
        )
        scout = g.add_node(MyScout, behalf_of=noticer, problem=tag)

        self.assertTrue(g.already_built(
            MyScout, behalf_of=noticer, problem=tag
        ))

    def test_ac_build_op_result(self):
        class Builder(AcNode):
            acs = [
                LookForTup(
                    [Brick(4), Brick(5)],
                    within=InWorkspace,
                    asgn_to='operands'
                ),
                BuildOpResult(operands='operands')
            ]
            
        g = NumboGraph(Numble([4, 5, 6], 15))
        plus = g.look_for(Plus, within=g.ws)
        b4 = g.look_for(Brick(4), within=g.ws)
        b5 = g.look_for(Brick(5), within=g.ws)
        builder = g.add_node(Builder, member_of=g.ws, opclass=plus)

        g.do_timestep(actor=builder)

        block = g.as_node(g.look_for(Block, within=g.ws))
        new_plus = g.neighbor(block, 'source')
        self.assertTrue(g.is_of_class(new_plus, Plus))
        self.assertEqual(block, Block(9))
        self.assertCountEqual(
            g.neighbors(new_plus, 'operands'),
            as_nodeids([b4, b5])
        )

    def test_sleep_and_wake(self):
        g = NumboGraph(Numble([4, 5, 6], 15))
        assert g.t == 0, f't == {g.t}, should == 0'
        node = g.as_node(g.look_for(NoticeSolved))
        assert node, "We need an ActiveNode for this unit test."
        self.assertTrue(node.id in g.active_nodes())
        node.state = Sleeping(Start, until=4)
        self.assertFalse(node.id in g.active_nodes())

        g.do_timestep()
        assert g.t == 1
        self.assertFalse(node.id in g.active_nodes())

        g.do_timestep()
        assert g.t == 2
        self.assertFalse(node.id in g.active_nodes())

        g.do_timestep()
        assert g.t == 3
        self.assertTrue(node.id in g.active_nodes())
            # Expect True because node will be active on next timestep
        
    def test_build_agent_for_needarg(self):
        # Here we test the entire sequence of becoming blocked for a missing
        # argument, posting a FillParamScout to fill it in, and running
        # successfully with the filled-in argument.
        g = NumboGraph(Numble([4, 5, 6], 15))
        bricks = g.find_all(OfClass(Brick))
        glom = g.add_node(Glom, g.find_all(OfClass(Brick)))
        noticer = g.add_node(NoticeAllBricksAreAvail, member_of=g.ws)
        assert len(bricks) == 3

        # First, the Noticer tries to run, but can't, because it's missing
        # a 'within' argument. So, it posts a Blocked tag about it:
        g.do_timestep(actor=noticer)
        problem_tag = g.neighbor(noticer, neighbor_class=Blocked)
        assert problem_tag, 'Noticer did not create problem tag.'

        # Next, the Noticer should build an agent to fix the problem.
        self.assertCountEqual(
            as_iter(g.actions(noticer)),
            [BuildAgent(noticer, problem_tag)]
        )
        g.do_timestep(actor=noticer)
        scout = g.neighbor(noticer, 'agents')
        self.assertTrue(
            g.is_of_class(scout, FillParamScout),
            'Noticer did not build a FillParamScout'
        )

        # Now the Noticer should be blocked:
        self.assertTrue(g.is_blocked(noticer))
        ## and have nothing to do:
        #self.assertFalse(g.actions(noticer))
        # and should only want to boost the scout:
        self.assertEqual(g.actions(noticer), BoostFromTo({scout}))

        # The Scout should find the Glom, override the Noticer's 'within'
        # arg with it, and remove the Blocked tag.
        g.do_timestep(actor=scout)

        # Therefore the Noticer should no longer be blocked:
        self.assertFalse(g.is_blocked(noticer))
        # and the Noticer should be able to act:
        self.assertTrue(g.actions(noticer))

        # Finally, the Noticer notices what it's looking for:
        g.do_timestep(actor=noticer)
        self.assertTrue(g.has_tag(bricks, AllBricksAvail))

        self.assertTrue(g.is_sleeping(noticer))
#        self.assertTrue(g.is_active(noticer))  # Noticer should stay active
#                                               # even after success
        # TODO self.assertEqual(g.get(noticer, 'within'), glom)

    def test_oom_and_gt(self):
        g = NumboGraph(Numble([10, 5], 15))
        b10 = g.look_for(Brick(10))
        b5 = g.look_for(Brick(5))
        t15 = g.look_for(Target(15))
        assert b10, "Couldn't find Brick(10)"
        assert b5, "Couldn't find Brick(5)"
        assert t15, "Couldn't find Target(15)"
        oomtagger = g.add_node(OoMTagger, member_of=g.ws)
        oomgttagger = g.add_node(OoMGreaterThanTagger, member_of=g.ws)

        g.do_timestep(actor=oomtagger)
        g.do_timestep(actor=oomtagger)
        g.do_timestep(actor=oomtagger)
        #pg(g)

        self.assertEqual(g.as_node(g.tag_of(b10, OoM)), OoM(value=1.0))
        self.assertEqual(g.as_node(g.tag_of(b5, OoM)), OoM(value=log10(5)))
        self.assertEqual(g.as_node(g.tag_of(t15, OoM)), OoM(value=log10(15)))

        # TODO Assert that this fizzles.
        g.do_timestep(actor=oomtagger)

        ShowPrimitives.start_logging()
        g.do_timestep(actor=oomgttagger, num=6)
        pg(g, oomgttagger)
        self.assertTrue(g.has_tag(t15, OoMGreaterThan, lesser=b5, greater=t15))
        pg(g)

if __name__ == '__main__':
    g = NumboGraph(Numble([10, 5], 15))
    b10 = g.look_for(Number(10))
