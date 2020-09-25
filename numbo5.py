# numbo5.py -- Rebuilding Numbo from scratch again, this time starting with
#              1 1 1 1 1; 5.

from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar
from operator import add, mul
from functools import reduce
from copy import copy, deepcopy

from codegen import make_python, compile_fargish
from dataclasses import dataclass
#from TimeStepper import TimeStepper
from log import *
import expr
from util import as_iter, reseed, intersection, first
#from PortGraph import PortGraph, Node, pg, ps, pa
#import WithActivation
#import support
from Numble import make_numble_class, prompt_for_numble
from ExprAsEquation import ExprAsEquation
from Action import Action, Build, make_build, Raise, SelfDestruct, \
    NEWBuild
from ActiveNode import ActiveNode, ActionNode, make_action_sequence, Start, \
    Completed
from BuildSpec import make_buildspec
from criteria import Tagged, HasValue, OfClass, NotTaggedTogetherWith, \
    HasAttr, NotNode, Criterion, Activated
from exc import NeedArg, FargDone
from Predefs import AllTagged

from StdGraph import Graph, pg
from Node import Node

prog = '''
gfuncs { succeeded }

tags -- taggees
target -- tags
members -- member_of
operands -- consumer
result_consumer -- source  # HACK: should be 'consumer'; see unique_mate().

Workspace

Tag(taggees)
Want, Avail, Consumed, Allowed, Done : Tag
#AllBricksAvail : Tag
SameValue, AllMembersSameValue : Tag

Group(members)
Glom : Group

Number(value)
Brick, Target, Block(source, consumer) : Number
Count : Tag #, Number

Operator(operands, consumer)
Plus, Times : Operator
Minus(minuend, subtrahend) : Operator

SuccessScout(target)
  see winner := NodeWithValue(target.value, nodeclass=Number, tagclass=Avail)
  => succeeded(winner, target)
'''

make_python(prog, debug=1)
exec(compile_fargish(prog, saveto='numbo5.gen.py'), globals())

Plus.expr_class = expr.Plus #HACK
Plus.symbol = '+' #HACK
Times.expr_class = expr.Times #HACK
Times.symbol = '*' #HACK

Numble = make_numble_class(
    Brick, Target, Want, Avail, Allowed, [Plus, Times, Minus]
)

##### Hacks

#tag_port_label = 'taggees'
#taggee_port_label = 'tags'
#
#@classmethod
#def cls_add_tag(cls, g, taggees):  # HACK
#    taggees = list(as_iter(taggees))
#    taggee_containers = intersection(
#        *[g.member_of(ee) for ee in as_iter(taggees)]
#    )
#    tag = g.add_node(cls, container=taggee_containers)
#    for taggee in as_iter(taggees):
#        g.add_edge(tag, tag_port_label, taggee, taggee_port_label)
#    return tag
#
#Tag.add_tag = cls_add_tag  # HACK

##### Custom Actions

@dataclass
class ActivateSlipnode(Action):
    slipnode: int

    def go(self, g):
        ws = g.ws
        new_node = g.copy_group(self.slipnode, ws)
        g.datum(new_node).min_activation = 6.0
        g.set_activation(new_node, 6.0)
        g.set_activation(self.slipnode, 0.1)

@dataclass
class SeekAndGlom(Action):
    criteria: Union[Criterion, List[Criterion], None]
    within: Union[int, None]=None

    threshold = 1.0

    def go(self, g):
        glommees = g.find_all(*as_iter(self.criteria), within=self.within)
        if glommees:
            g.do(Build.maybe_make(g, Glom, [glommees], {}))
            g.new_state(self.actor, Completed)
        # TODO else: FAILED

@dataclass
class NoticeAllSameValue(Action):
    value: Any
    within: Union[int, None]=None

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
    within: Union[int, None]=None

    threshold = 1.0

    def go(self, g):
        if not self.within:
            raise NeedArg(self, 'within')
        num_members = len(g.neighbors(self.within, port_label='members'))
        g.add_node(Count, taggees=[self.within], value=num_members)
        g.new_state(self.actor, Completed)

@dataclass
class NoticeSameValue(Action):
    node1: Union[int, None]
    node2: Union[int, None]

    threshold: float = 1.0

    def go(self, g):
        if not self.node1:
            raise NeedArg(self, 'node1')
        if not self.node2:
            raise NeedArg(self, 'node2')
        if g.value_of(self.node1) == g.value_of(self.node2):
            g.do(
                Build.maybe_make(
                    g, SameValue, [], dict(taggees=[self.node1, self.node2])
                )
            )
            g.new_state(self.actor, Completed)

@dataclass
class AddAllInGlom(Action):
    within: Union[int, None]=None

    threshold: float = 1.0

    def go(self, g):
        if not self.within:
            raise NeedArg(self, 'within')
        g.consume_operands(
            g.find_all(OfClass(Number), within=self.within),
            Plus
        )
        g.new_state(self.actor, Completed)

@dataclass
class SeekArg(Action):
    for_node: int
    port_label: str
    nodeclass: Type[Node]
    
    def go(self, g):
        found_node = g.look_for(OfClass(self.nodeclass))
        if found_node:
            g.add_override_node(self.for_node, self.port_label, found_node)
            g.boost_salience(self.for_node)
            g.new_state(self.actor, Completed)
            g.remove_node(g.neighbors(self.actor, 'rm_on_success'))

@dataclass
class StartScout(Action):
    action: Action
    rm_on_success: Union[int, None]=None

    def go(self, g):
        g.add_node(
            ActionNode,
            action=self.action,
            rm_on_success=self.rm_on_success
        )
        g.new_state(self.actor, Completed)


##### Nodeclasses defined in Python

is_number = OfClass(Number)

class Slipnet(Group, ActiveNode):

    min_support_for = 1.0
    min_activation = 2.0

    def actions(self, g):
        actives = g.find_all(
            Activated(), OfClass(ActiveNode),
            subset=g.members_of(self)
            #TODO within=self ?
        )
        print('ACTIVE SLIPNODES', actives)
        return [ActivateSlipnode(slipnode) for slipnode in actives]

class AllBricksAvail(Tag, ActiveNode):

    initial_support_for = 1.0
    initial_activation = 10.0  # HACK

    def actions(self, g):
        bricks = g.find_all(OfClass(Brick))
        if not AllTagged(g, Avail, bricks):
            return [SelfDestruct(self)]

    def on_build(self):
        self.g.set_mutual_activation(
            self, self.g.find_archetype(self), weight=1.0
        )

class NoticeAllBricksAvail(ActiveNode):

    min_support_for = 1.0

    def actions(self, g):
        bricks = g.find_all(OfClass(Brick))
        if AllTagged(g, Avail, bricks):
            return NEWBuild.maybe_make(g, AllBricksAvail, taggees=bricks)
#        bricks = g.find_all(OfClass(Brick))
#        buildspec = make_buildspec(
#            g, AllBricksAvail, kwargs=dict(taggees=bricks)
#        )
#        if not g.is_already_built(buildspec):
#            if AllTagged(g, Avail, bricks):
#                return [Build(buildspec)]
                #TODO When a Brick is no longer Avail, this tag needs to get
                #removed.
        
class SameNumberGlommer(ActiveNode):
    
    def actions(self, g):
        number_node = g.look_for(is_number)
        all_with_same_value = g.find_all(
            is_number, HasValue(g.value_of(number_node))
        )
        #return [make_build3(g, Glom, [all_with_same_value], {})]
        return Build.maybe_make(g, Glom, [all_with_same_value], {})

class MemberCounter(ActiveNode):

    def actions(self, g):
        group_node = g.look_for(OfClass(Group))
        if group_node is None:
            return
        num_members = len(g.neighbors(group_node, port_label='members'))
        return [make_build3(g, Count, [], {
            'taggees': [group_node], 'value': num_members
        })]

class SameValueTagger(ActiveNode):

    def actions(self, g):
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

    port_label_to_nodeclass = dict(   # HACK
        within=Glom,
        node1=Target,
        node2=Count,
    )

    def actions(self, g):
        return [
            StartScout(
                action=SeekArg(
                    g.neighbor(self, 'taggees'),
                    self.reason.name,  # HACK: assumes NeedArg
                    self.port_label_to_nodeclass[self.reason.name]  # HACK
                ),
                rm_on_success=self
            )
        ]


##### The graph class and other generic execution code #####

#class DemoGraph(TimeStepper, ExprAsEquation, PortGraph):
class DemoGraph(ExprAsEquation, Graph):
#    port_mates = port_mates
#    nodeclasses = nodeclasses
#    nodeclasses['Failed'] = Failed
#
#    default_graph_attrs = dict(
#        t=0,
#        done=False,
#        num_timesteps=40,
#        seed=None,
#        running=False,
#        port_mates=port_mates,
#        support_propagator=support.Propagator(
#            max_total_support=70,  #300
#            positive_feedback_rate=0.1,
#            sigmoid_p=0.5,
#            alpha=0.95
#        ),
#        activation_propagator=WithActivation.Propagator(
#            max_total_activation=20,
#            sigmoid_p=0.5,
#            alpha=0.98,
#            # TODO noise=0.0
#        )
#    )

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

        # Make initial nodes

        ws = self.add_node(Workspace)

        slipnet = self.add_node(Slipnet)
        self.fill_slipnet(slipnet)

        numble.build(self, ws)

        #HACK
        #self.add_node(SameNumberGlommer)
        #self.add_node(MemberCounter)
        #self.add_node(SameValueTagger)

        targetid = self.look_for(OfClass(Target))
        self.add_node(SuccessScout, target=targetid, min_support_for=1.0)
        self.add_node(NoticeAllBricksAvail, member_of=ws)

    def fill_slipnet(self, slipnet: int):
        seqnode = make_action_sequence(
            self,
            SeekAndGlom(within=ws, criteria=OfClass(Brick)),
            NoticeAllSameValue(value=1, within=None),
            CountMembers(within=None),
            NoticeSameValue(node1=None, node2=None),
            AddAllInGlom(),
            member_of=slipnet,
        )
        aba = self.add_node(AllBricksAvail, member_of=slipnet)
        self.set_activation_from_to(aba, seqnode, weight=1.0)

    def find_archetype(self, node):
        return self.find_all(
            OfClass(self.class_of(node)), subset=self.members_of(self.slipnet)
            # TODO within?
        )

    def consume_operands(
        self,
        operand_ids: List[int],
        operator_class: Operator,
        actor=None
    ):
        # Check that all the nodes are Avail
        operator_id = self.add_node(
            operator_class, 
            operands=operand_ids,
            builder=actor,
        )
        result_id = self.add_node(
            Block,
            value=arith_result0(self, operator_class, operand_ids),
            source=operator_id,
            builder=actor,
        )
        g.move_tag(Avail, operand_ids, result_id)
        g.add_tag(Consumed, operand_ids)
        g.add_tag(Done, actor)

def arith_result(g, operator_id):
    operator_class = g.class_of(operator_id)
    #print('OCLASS', operator_class)
    if operator_class == Minus: #HACK
        return 0.0  # STUB
    else:
        operand_ids = g.neighbors(operator_id, port_label='operands')
        return arith_result0(g, operator_class, operand_ids)

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

class NumboSuccess(FargDone):
    succeeded = True

    def __init__(self, expr):
        self.expr = expr

    def __str__(self):
        return 'Success!  ' + str(self.expr)

def succeeded(g, winnerid, targetid):
    return Raise(NumboSuccess,
                 expr.Equation(
                   extract_expr(g, winnerid),
                   extract_expr(g, targetid)))

def extract_expr(g, nodeid):
    '''Extracts an Expr tree consisting of nodeid and its sources.'''
    nodeclass = g.class_of(nodeid)
    if issubclass(nodeclass, Block):
        return extract_expr(g, g.neighbor(nodeid, 'source'))
    elif issubclass(nodeclass, Number):
        return expr.Number(g.value_of(nodeid))
    elif issubclass(nodeclass, Operator):
        operand_exprs = (
            extract_expr(g, n)
                for n in g.neighbors(nodeid, ['source', 'operands'])
        )
        return g.datum(nodeid).expr_class(*operand_exprs)
    else:
        raise ValueError(f'extract_expr: node {nodeid} has unrecognized class {nodeclass}')

def new_graph(numble, seed=None):
    g = DemoGraph(numble=numble, seed=seed)
    return g
    
g = None
ws = None

def newg():
    global g, ws
    g = new_graph(Numble([1, 1, 1, 1, 1], 5), seed=8028868705202140491)
    ws = g.ws
    return g

def p():
    '''Print just the four nodes of interest for the demo.'''
    pg(g, [20, 21, 22, 23, 24])

if __name__ == '__main__':
    ShowAnnotations.start_logging()
    ShowActionList.start_logging()
    ShowActionsChosen.start_logging()
    #ShowIsMatch.start_logging()

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

    g.do_timestep(num=1)
    pg(g)
    #print("\nMANUAL ACTION HERE: activating slipnode for 'Notice that all the bricks are 1, count them up, and notice that the count equals the target, and add up the bricks.\n")
    #g.copy_group(8, 1)  # HACK to activate ActionSeqNode from slipnet
    #g.do_timestep(num=29)
    #pg(g)


    #dt = g.datum(3)
    #dt2 = copy(dt)
    #kwargs = {'action': SeekAndGlom(criteria=OfClass(Brick), within=None), 'state': Start}
    #an = ActionNode(**kwargs)
