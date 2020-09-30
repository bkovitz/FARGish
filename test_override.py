# test_override.py -- Unit tests for overriding Action arguments

import unittest
from pprint import pprint as pp
import inspect
from dataclasses import dataclass
import dataclasses
from typing import Union, List, Any

from codegen import make_python, compile_fargish
from Numble import make_numble_class, prompt_for_numble
from Action import Action, Build
from ActiveNode import ActionNode, ActionSeqNode, Start, Dormant, Completed, \
    make_action_sequence
from StdGraph import Graph, pg
#from Node import Node
from log import *
from TimeStepper import TimeStepper
import support
from util import reseed
from criteria import Tagged, HasValue, OfClass, NotTaggedTogetherWith, \
    HasAttr, NotNode, Criterion
from exc import NeedArg


prog = '''
tags -- taggees
target -- tags
members -- member_of

Workspace

Tag(taggees)
Want, Avail, Allowed : Tag
SameValue, AllMembersSameValue : Tag
Failed(reason) : Tag

Group(members)
Glom : Group

Number(value)
Brick, Target, Block : Number
Count : Tag, Number

Operator
Plus, Times : Operator
Minus(minuend, subtrahend) : Operator
'''

make_python(prog) #, debug=1, file=open('test_override.gen.py', 'w'))
exec(compile_fargish(prog), globals())

Numble = make_numble_class(
    Brick, Target, Want, Avail, Allowed, [Plus, Times, Minus]
)

##### Hacks

tag_port_label = 'taggees'
taggee_port_label = 'tags'

@classmethod
def cls_add_tag(cls, g, taggees):  # HACK
    taggees = list(as_iter(taggees))
    taggee_containers = intersection(
        *[g.member_of(ee) for ee in as_iter(taggees)]
    )
    tag = g.add_node(cls, container=taggee_containers)
    for taggee in as_iter(taggees):
        g.add_edge(tag, tag_port_label, taggee, taggee_port_label)
    return tag

Tag.add_tag = cls_add_tag  # HACK

##### Custom Actions

@dataclass
class SeekAndGlom(Action):
    criteria: Union[Criterion, List[Criterion], None]
    within: int

    threshold = 1.0

    def go(self, g):
        glommees = g.find_all(*as_iter(self.criteria), within=self.within)
        if glommees:
            g.do(Build.maybe_make(g, Glom, glommees))
            g.new_state(self.actor, Completed)
        # TODO else: FAILED

@dataclass
class NoticeAllSameValue(Action):
    within: Union[int, None]
    value: Union[Any, None]

    threshold: float = 1.0

    def go(self, g):
        # Test that all members of 'within' have value 'value'.
        # If so, tag 'within' AllMembersSameValue

        if not self.within:
            raise NeedArg(self, 'within')
        if all(
            g.value_of(memberid) == self.value
                for memberid in g.members_of(self.within)
        ):
            g.do(Build.maybe_make(g, AllMembersSameValue, self.within))
            g.new_state(self.actor, Completed)
        # TODO else: FAILED


class TestGraph(Graph):
    port_mates = port_mates
    nodeclasses = nodeclasses

    default_graph_attrs = dict(
        t=0,
        done=False,
        num_timesteps=40,
        seed=None,
        running=False,
        port_mates=port_mates,
        support_propagator=support.Propagator(
            max_total_support=70,  #300
            positive_feedback_rate=0.1,
            sigmoid_p=0.5,
            alpha=0.95
        )
    )

    def __init__(self, numble, *args, **kwargs):
        super().__init__(*args, **kwargs)
#        kws = self.default_graph_attrs.copy()
#        kws.update(kwargs)
#        if kws.get('num_timesteps', None) is None:
#            kws['num_timesteps'] = self.default_graph_attrs['num_timesteps']
#        kws['seed'] = reseed(kws.get('seed', None))
#        super().__init__(**kws)
#        self.consecutive_timesteps_with_no_response = 0
        self.nodeclasses.update(nodeclasses)
        self.port_mates += port_mates

        ws = self.add_node(Workspace)
        numble.build(self, ws)
#        if 'numble' in self.graph:
#            self.graph['numble'].build(self, ws)


def new_graph(numble=Numble([1, 1, 1, 1, 1], 5), seed=8028868705202140491):
    return TestGraph(numble=numble, seed=seed)

#ShowAnnotations.start_logging()
#ShowActionList.start_logging()
#ShowActionsChosen.start_logging()
#ShowActionsPerformed.start_logging()
#ShowIsMatch.start_logging()

class TestOverride(unittest.TestCase):

    def test_override(self):
        g = new_graph()
        g.do_timestep(action=SeekAndGlom(
            criteria=OfClass(Brick),
            within=g.ws
        ))
        glom = g.look_for(OfClass(Glom))
        assert glom is not None

        # Action lacks 'within' arg
        noticer = g.add_node(
            ActionNode,
            NoticeAllSameValue(within=None, value=1, threshold=0.0),
            min_support_for=1.0,
            member_of=g.ws
        )

        # There are no overrides yet
        param_names = g.datum(noticer).action.param_names()
        self.assertEqual(param_names, {'within', 'value', 'threshold'})
        self.assertEqual(g.get_overrides(noticer, param_names), {})

        # Verify that nothing happens without override
        g.do_timestep()
        self.assertEqual(g.nodes_of_class(AllMembersSameValue), [])

        # Override 'within' by linking from ActionNode's 'within' port
        g.add_override_node(noticer, 'within', glom)
        new_action = g.datum(noticer).action.with_overrides_from(g, noticer)
        self.assertEqual(new_action.within, glom)

        # Also remove the Failed tag
        g.remove_tag(noticer, Failed)

        # Let the noticer run again: it should tag the Glom this time
        g.do_timestep()
        #pg(g)
        tags = g.find_all(OfClass(AllMembersSameValue))
        self.assertEqual(len(tags), 1,
            'Failed to tag the glom with AllMembersSameValue'
        )
        # TODO
#        self.assertTrue(g.is_built_by(tags[0], noticer),
#            'The AllMembersSameValue tag has not been marked as built by the Noticer.'
#        )

    def test_failed_tag(self):
        g = new_graph()
        g.do_timestep(action=SeekAndGlom(
            criteria=OfClass(Brick),
            within=g.ws
        ))
        glom = g.look_for(OfClass(Glom))
        assert glom is not None

        # Action lacks 'within' arg
        noticer = g.add_node(
            ActionNode,
            NoticeAllSameValue(within=None, value=1, threshold=0.0)
        )

        # ...So performing it should make a Failed tag
        g.do_timestep(actor=noticer)
        tag = g.tag_of(noticer, Failed)
        self.assertTrue(g.is_tag(tag))
        self.assertTrue(g.is_of_class(tag, Failed))
        reason = g.value_of(tag, 'reason')
        self.assertTrue(isinstance(reason, NeedArg))
        self.assertEqual(reason.name, 'within')
        self.assertTrue(g.is_built_by(tag, noticer))

        # And with a Failed tag, the noticer should not generate an action
        self.assertTrue(g.is_dormant(noticer))

    def test_no_dup_failed(self):
        # Make sure that NoticeAllSameValue only gets one Failed tag no
        # matter how many times it fails.
        g = new_graph()
        g.do_timestep(action=SeekAndGlom(
            criteria=OfClass(Brick),
            within=g.ws
        ))
        glom = g.look_for(OfClass(Glom))
        assert glom is not None

        # Action lacks 'within' arg
        noticer = g.add_node(
            ActionNode,
            NoticeAllSameValue(within=None, value=1, threshold=0.0),
            min_support_for=1.0
        )

        # Verify that nothing happens without override
        g.do_timestep(actor=noticer)
        g.do_timestep(actor=noticer)
        faileds = g.neighbors(noticer, port_label='tags', neighbor_class=Failed)
        self.assertEqual(len(faileds), 1)
