import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Callable

from Ac import Ac, AcNode, AdHocAcNode, All, AllAre, TagWith, AddNode, OrFail, \
    MembersOf, Len, EqualValue, Taggees, LookFor, Raise, PrintEnv, AcNot, \
    SelfDestruct, FindParamName, LookForArg, AddOverride, RemoveBlockedTag
from codegen import make_python, compile_fargish
from criteria import OfClass, Tagged as CTagged, HasThisValue
from StdGraph import Graph, MyContext, pg
from Numble import make_numble_class
from testNumboClasses import *
from Node import Node, NRef, NRefs, CRef, MaybeNRef
from ActiveNode import ActiveNode, Start, Completed, HasUpdate
from Action import Action, Actions, BuildAgent
from log import *
from util import first
from exc import AcNeedArg, ActionFailure, AcFailed, FargDone, NeedArg

class TestAc(unittest.TestCase):

    def setUp(self):
        stop_all_logging()

    def test_do_notice_and_tag(self):
        # Tests running Ac objects directly. Normally, though, you only run
        # Ac objects from inside an AcNode.
        g = NumboTestGraph(Numble([4, 5, 6], 15))
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
        g = NumboTestGraph(Numble([4, 5, 6], 15))
        
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
        g = NumboTestGraph(Numble([4, 5, 6], 15))
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
        g = NumboTestGraph(Numble([4, 5, 6], 15))
        bricks = g.find_all(OfClass(Brick))

        seek_and_glom = g.add_node(SeekAndGlom, member_of=g.ws, within=g.ws)

        g.do_timestep(actor=seek_and_glom)
        glom = g.look_for(
            OfClass(Glom), subset=g.neighbors(bricks, 'member_of')
        )

        self.assertTrue(glom, 'Did not build Glom')
        self.assertCountEqual(g.neighbors(glom, 'members'), bricks)

    # TODO test overriding 'value' with the value of another node
    def test_ac_has_value(self):
        g = NumboTestGraph(Numble([3, 3, 3], 15))
        glom = g.add_node(Glom, g.find_all(OfClass(Brick)))

        noticer = g.add_node(
            NoticeAllHaveThisValue, member_of=g.ws, within=glom
        )

        g.do_timestep(actor=noticer)
        self.assertTrue(g.has_tag(glom, AllMembersHaveThisValue))

    def test_ac_has_value_fail(self):
        g = NumboTestGraph(Numble([4, 5, 6], 15))
        glom = g.add_node(Glom, g.find_all(OfClass(Brick)))

        noticer = g.add_node(
            NoticeAllHaveThisValue, member_of=g.ws, within=glom
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

        g = NumboTestGraph(Numble([4, 5, 6], 15))
        glom = g.add_node(Glom, g.find_all(OfClass(Brick)))

        self.assertFalse(g.has_tag(glom, Count(value=3)))

        counter = g.add_node(CountMembers, within=glom)
        g.do_timestep(actor=counter)
        self.assertTrue(g.has_tag(glom, Count(value=3)))

    def test_ac_notice_same_value(self):
        g = NumboTestGraph(Numble([1, 1, 1], 3))
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
        g = NumboTestGraph(Numble([4, 5, 6], 15))
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

        g = NumboTestGraph(Numble([4, 5, 6, 15], 15))
        target = g.look_for(OfClass(Target))
        glom = g.add_node(Glom, g.find_all(OfClass(Brick)))

        noticer = g.add_node(NoticeSolved, within=glom, target=target)

        g.do_timestep(actor=noticer)

        got = g.done()
        self.assertEqual(got.__class__, NumboSuccess)
        self.assertEqual(g.as_node(got.node), Brick(15))
        self.assertEqual(g.as_node(got.target), Target(15))

    def test_ac_selfdestruct_on_update(self):
        g = NumboTestGraph(Numble([4, 5, 6], 15))
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
        g = NumboTestGraph(Numble([4, 5, 6], 15))
        glom = g.add_node(Glom, g.find_all(OfClass(Brick)))
        noticer = g.add_node(NoticeAllBricksAreAvail, member_of=g.ws)
        tag = g.add_tag(
            Blocked(reason=NeedArg(noticer.action, 'within')),
            noticer
        )
        scout = g.add_node(FillParamScout, behalf_of=noticer, problem=tag)
        g.do_timestep(actor=scout)

        # The FillParamScout should do the override:
        self.assertTrue(g.has_hop(noticer, 'within', glom, 'overriding'))

        # and remove the Blocked tag:
        self.assertFalse(g.has_node(tag))

    def test_build_agent_for_needarg(self):
        # Here we test the entire sequence of becoming blocked for a missing
        # argument, posting a FillParamScout to fill it in, and running
        # successfully with the filled-in argument.
        g = NumboTestGraph(Numble([4, 5, 6], 15))
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
            [BuildAgent(noticer, {problem_tag})]
        )
        g.do_timestep(actor=noticer)
        scout = g.neighbor(noticer, 'agents')
        self.assertTrue(
            g.is_of_class(scout, FillParamScout),
            'Noticer did not build a FillParamScout'
        )

        # Now the Noticer should be blocked:
        self.assertTrue(g.is_blocked(noticer))
        # and have nothing to do:
        self.assertFalse(g.actions(noticer))

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

        # TODO self.assertEqual(g.get(noticer, 'within'), glom)
