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
from Node import NRef
from log import *
#import support
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
Blocked(reason) : Tag

Group(members)
Glom : Group

Number(value)
Brick, Target, Block : Number
Count : Tag, Number

Operator
Plus, Times : Operator
Minus(minuend, subtrahend) : Operator
'''

#make_python(prog) #, debug=1, file=open('test_override.gen.py', 'w'))
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
    focal_point: int

    threshold = 1.0

    def go(self, g, actor):
        glommees = g.find_all(*as_iter(self.criteria), focal_point=self.focal_point)
        if glommees:
            g.do(Build.maybe_make(g, Glom, glommees))
            g.new_state(actor, Completed)
        # TODO else: FAILED

@dataclass
class NoticeAllSameValue(Action):
    focal_point: Union[int, None]
    value: Union[Any, None]

    threshold: float = 1.0

    def go(self, g, actor):
        # Test that all members of 'focal_point' have value 'value'.
        # If so, tag 'focal_point' AllMembersSameValue

        if not self.focal_point:
            raise NeedArg(ac=self, name='focal_point')
        if all(
            g.value_of(memberid) == self.value
                for memberid in g.members_of(self.focal_point)
        ):
            g.do(Build.maybe_make(g, AllMembersSameValue, self.focal_point))
            g.new_state(actor, Completed)
        # TODO else: FAILED


class TestGraph(Graph):
    port_mates = port_mates
    nodeclasses = nodeclasses

    def __init__(self, numble, *args, **kwargs):
        super().__init__(*args, **kwargs)
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
            focal_point=g.ws
        ))
        glom = g.look_for(OfClass(Glom))
        assert glom is not None

        # Action lacks 'focal_point' arg
        noticer = g.add_node(
            ActionNode,
            NoticeAllSameValue(focal_point=None, value=1, threshold=0.0),
            min_support_for=1.0,
            member_of=g.ws
        )

        # There are no overrides yet
        param_names = g.datum(noticer).action.param_names()
        self.assertEqual(param_names, {'focal_point', 'value', 'threshold'})
        self.assertEqual(g.get_overrides(noticer, param_names), {})

        # Verify that nothing happens without override
        g.do_timestep()
        self.assertEqual(g.nodes_of_class(AllMembersSameValue), [])

        # Override 'focal_point' by linking from ActionNode's 'focal_point' port
        g.add_override_node(noticer, 'focal_point', glom)
        new_action = g.datum(noticer).action.with_overrides_from(g, noticer)
        self.assertEqual(new_action.focal_point, glom)

        # Also remove the Blocked tag
        g.remove_tag(noticer, Blocked)

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

    def test_blocked_tag(self):
        g = new_graph()
        g.do_timestep(action=SeekAndGlom(
            criteria=OfClass(Brick),
            focal_point=g.ws
        ))
        glom = g.look_for(OfClass(Glom))
        assert glom is not None

        # Action lacks 'focal_point' arg
        noticer = g.add_node(
            ActionNode,
            NoticeAllSameValue(focal_point=None, value=1, threshold=0.0)
        )

        # ...So performing it should make a Blocked tag
        g.do_timestep(actor=noticer)
        tag = g.tag_of(noticer, Blocked)
        self.assertTrue(g.is_tag(tag))
        self.assertTrue(g.is_of_class(tag, Blocked))
        reason = g.value_of(tag, 'reason')
        self.assertTrue(isinstance(reason, NeedArg))
        self.assertEqual(reason.name, 'focal_point')
        self.assertTrue(g.is_built_by(tag, noticer))

        self.assertTrue(g.is_blocked(noticer))
