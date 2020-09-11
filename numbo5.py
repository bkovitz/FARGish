# numbo5.py -- Rebuilding Numbo from scratch again, this time starting with
#              1 1 1 1 1; 5.

from typing import Union, List, Any

from codegen import make_python, compile_fargish
from dataclasses import dataclass
from TimeStepper import TimeStepper
from log import *
from util import as_iter, reseed, intersection, first
from PortGraph import PortGraph, Node, pg, ps
import support
from Numble import make_numble_class, prompt_for_numble
from ExprAsEquation import ExprAsEquation
from Action import Action, Build, make_build
from ActiveNode import ActiveNode, make_action_sequence, Completed
from BuildSpec import make_buildspec
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

Group(members)
Glom : Group

Number(value)
Brick, Target, Block : Number
Count : Tag, Number

Operator
Plus, Times : Operator
Minus(minuend, subtrahend) : Operator
'''

make_python(prog, debug=1)
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
    tag = g.make_node(cls, container=taggee_containers)
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
            g.do(Build.maybe_make(g, Glom, [glommees], {}))
            g.new_state(self.actor, Completed)
        # TODO else: FAILED

@dataclass
class NoticeAllSameValue(Action):
    within: Union[int, None]
    value: Any

    threshold = 1.0

    def go(self, g):
        # Test that all members of 'within' have value 'value'.
        # If so, tag 'within' AllMembersSameValue

        # HACK  Need to find 'within' node left by SeekAndGlom or whatever
        # process set up the node in which we should NoticeAllSameValue.
        #within = first(g.prev_new_nodes)
#        if not self.within:
#            self.within = g.look_for(OfClass(Glom))
#            if not self.within:
#                return # TODO FAIL
        if not self.within:
            raise NeedArg(self, 'within')
        if all(
            g.value_of(memberid) == self.value
                for memberid in g.members_of(self.within)
        ):
            g.do(Build.maybe_make(g, AllMembersSameValue, [self.within], {}))
            g.new_state(self.actor, Completed)
        # TODO else: FAILED

@dataclass
class CountMembers(Action):
    within: Union[int, None]

    threshold = 1.0

    def go(self, g):
        if not self.within:  # HACK
            self.within = g.look_for(OfClass(Glom))
            if not self.within:
                return # TODO FAIL
        num_members = len(g.neighbors(self.within, port_label='members'))
        g.make_node(Count, taggees=[self.within], value=num_members)
        g.new_state(self.actor, Completed)

@dataclass
class NoticeSameValue(Action):
    node1: Union[int, None]
    node2: Union[int, None]

    threshold: float = 1.0

    def go(self, g):
        if not self.node2:   # HACK
            self.node2 = g.look_for(OfClass(Count))
            if not self.node2:
                return # TODO FAIL
        if g.value_of(self.node1) == g.value_of(self.node2):
            g.do(
                Build.maybe_make(
                    g, SameValue, [], dict(taggees=[self.node1, self.node2])
                )
            )
            g.new_state(self.actor, Completed)

@dataclass
class ArgScout(Action):
    for_node: int
    port_label: str
    nodeclass: Node
    
    def go(self, g):
        found_node = g.look_for(OfClass(self.nodeclass))
        if found_node:
            g.add_override_node(self.for_node, self.port_label, found_node)
            g.new_state(self.actor, Completed)
            g.boost_salience(self.for_node)

@dataclass
class StartScout(Action):
    pass

##### Nodeclasses defined in Python

is_number = OfClass(Number)
        
class SameNumberGlommer(ActiveNode):
    
    def actions(self, g, thisid):
        number_node = g.look_for(is_number)
        all_with_same_value = g.find_all(
            is_number, HasValue(g.value_of(number_node))
        )
        #return [make_build3(g, Glom, [all_with_same_value], {})]
        return Build.maybe_make(g, Glom, [all_with_same_value], {})

class MemberCounter(ActiveNode):

    def actions(self, g, thisid):
        group_node = g.look_for(OfClass(Group))
        if group_node is None:
            return
        num_members = len(g.neighbors(group_node, port_label='members'))
        return [make_build3(g, Count, [], {
            'taggees': [group_node], 'value': num_members
        })]

class SameValueTagger(ActiveNode):

    def actions(self, g, thisid):
        first_node = g.look_for(HasAttr('value'))
        if first_node is None:
            return
        value = g.value_of(first_node)
        second_node = g.look_for(
            NotNode(first_node),
            HasValue(value),
            NotTaggedTogetherWith(first_node, SameValue)
        )
        if second_node is None:
            return
        return make_build3(g, SameValue, [], {
            'taggees': [first_node, second_node],
            'value': value
        })

class Failed(ActiveNode, Tag):
    node_params = NodeParams(MateParam('taggees', 'tags'), AttrParam('reason'))

    def actions(self, g, thisid):
        pass


##### The graph class and other generic execution code #####

class DemoGraph(TimeStepper, ExprAsEquation, PortGraph):
    port_mates = port_mates
    nodeclasses = nodeclasses
    nodeclasses['Failed'] = Failed

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

    def __init__(self, **kwargs):
        super().__init__()
        kws = self.default_graph_attrs.copy()
        kws.update(kwargs)
        if kws.get('num_timesteps', None) is None:
            kws['num_timesteps'] = self.default_graph_attrs['num_timesteps']
        kws['seed'] = reseed(kws.get('seed', None))
        super().__init__(**kws)
        self.consecutive_timesteps_with_no_response = 0
        ws = self.make_node(Workspace)
        self.graph['ws'] = ws
        if 'numble' in self.graph:
            self.graph['numble'].build(self, ws)

        #HACK
        #self.make_node(SameNumberGlommer)
        #self.make_node(MemberCounter)
        #self.make_node(SameValueTagger)
        make_action_sequence(
            self,
            SeekAndGlom(within=ws, criteria=OfClass(Brick)),
            NoticeAllSameValue(value=1, within=None),
            CountMembers(within=None),
            NoticeSameValue(node1=self.look_for(OfClass(Target)), node2=None),
            min_support_for=6.0
        )

def new_graph(numble, seed=None):
    g = DemoGraph(numble=numble, seed=seed)
    return g
    
g = None
ws = None

def newg():
    global g, ws
    g = new_graph(Numble([1, 1, 1, 1, 1], 5), seed=8028868705202140491)
    ws = g.graph['ws']
    return g

def p():
    '''Print just the four nodes of interest for the demo.'''
    pg(g, [20, 21, 22, 23, 24])

ShowAnnotations.start_logging()
ShowActionList.start_logging()
ShowActionsChosen.start_logging()
#ShowIsMatch.start_logging()

if __name__ == '__main__':
    newg()

    #bspec = make_buildspec(g, Glom, [[4, 6, 8]], {})
    #bspec = make_buildspec(g, Glom, [], dict(taggees=[4, 6, 8]))
    #glom1 = bspec.build(g)
    #glom2 = bspec.build(g)  # Should be None
    #pg(g)
    #print(glom1, glom2)

    # Just run
#    g.do_timestep(num=5)
#    pg(g)
#    cr = OfClass(Number)

    # Force Glomming Bricks
#    g.do_action_sequence([
#        SeekAndGlom(OfClass(Brick), ws),
#        NoticeAllSameValue(value=1),
#    ])
#    cm = CountMembers(23)
#    g.do_action(cm)
#    pg(g)


#    g.do_action_sequence([
#        catcher(SeekAndGlom(OfClass(Brick), ws), built),
#        NoticeAllSameValue(within=catcher.get('built'), value=1),
#    ])



#    g.do_action_sequence([
#        SeekAndGlom(OfClass(Brick), ws),
#        SaveBuiltAs('glom'),
#        NoticeAllSameValue(within=Saved('glom'), value=1),
#    ])


    #g.do_timestep()
    #pg(g)
    # Now call g.do_timestep() 11 times and the model will "notice" that
    # all the Bricks are 1, and the number of Bricks = the Target.

    g.do_timestep(num=6)
    pg(g)
    # Now manually call  g.do(ArgScout(21, 'within', Glom))
    # Then  g.do_timestep()  and NoticeAllSameValue will succeed.
