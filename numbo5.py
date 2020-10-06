# numbo5.py -- Rebuilding Numbo from scratch again, this time starting with
#              1 1 1 1 1; 5.

from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar
from operator import add, mul
from functools import reduce
from copy import copy, deepcopy
import pdb

from codegen import make_python, compile_fargish
from dataclasses import dataclass
#from TimeStepper import TimeStepper
from log import *
import expr
from util import as_iter, as_set, reseed, intersection, first
#from PortGraph import PortGraph, Node, pg, ps, pa
#import WithActivation
#import support
from Numble import make_numble_class, prompt_for_numble
from ExprAsEquation import ExprAsEquation
from Action import Action, Actions, Build, Raise, SelfDestruct, FuncAction
from ActiveNode import ActiveNode, ActionNode, make_action_sequence, Start, \
    Completed
#from BuildSpec import make_buildspec
from criteria import Tagged as CTagged, NotTagged, HasValue, OfClass, \
    NotTaggedTogetherWith, HasAttr, NotNode, Criterion, Activated
from exc import NeedArg, FargDone, ActionFailure
from Predefs import AllTagged
from StdGraph import Graph
from ActiveGraph import pg, pa
from Node import Node, NRef, NRefs, MaybeNRef, PortLabel, CRef

prog = '''
gfuncs { succeeded }

tags -- taggees
target -- tags
members -- member_of
operands -- consumer
result_consumer -- source  # HACK: should be 'consumer'; see unique_mate().
consume_operands -- proposer
proposed_operator -- proposer

Workspace

Tag(taggees)
Avail, Consumed, Allowed, Done, Exclude : Tag
#AllBricksAvail : Tag
SameValue, AllMembersSameValue : Tag
Assessment : Tag
NotGoodEnough : Assessment
TooLow, TooHigh : NotGoodEnough
Want : Tag
  agent: AssessorScout(target=taggees)

Group(members)
Glom : Group

Number(value)
Brick, Target, Block(source, consumer) : Number
Count : Tag #, Number

Operator(operands, consumer)
Plus, Times : Operator
Minus(minuend, subtrahend) : Operator

#AssessorScout(target)
#  #see winner := NodeWithValue(target.value, nodeclass=Number, tagclass=Avail)
#  #=> succeeded(winner, target)
#  see node := And(Or(NodeOfClass(Brick), NodeOfClass(Block)),
#                  NodeWithTag(Avail),
#                  Not(NodeWithTag(Assessment)))
#  => assess(node, target)
'''

#make_python(prog, debug=1)
exec(compile_fargish(prog, saveto='numbo5.gen.py'), globals())

Operator.is_duplicable = True  #HACK

Plus.expr_class = expr.Plus #HACK
Plus.symbol = '+' #HACK
Times.expr_class = expr.Times #HACK
Times.symbol = '*' #HACK

Numble = make_numble_class(
    Brick, Target, Want, Avail, Allowed, [Plus, Times, Minus]
)

##### Hacks

Number.is_duplicable = True

Want.min_activation = 1.0

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

##### Custom Exceptions

@dataclass
class NotAllSameValue(ActionFailure):
    value: Any=None
    within: NRef=None

@dataclass
class NotSameValue(ActionFailure):
    node1: NRef=None
    node2: NRef=None

##### Action procedures

def assess(g, node: NRef, target: NRef) -> Actions:
    nv = g.value_of(node)
    if nv is None:
        return
    tv = g.value_of(target)
    if tv is None:
        return
    if nv == tv:
        return Raise(NumboSuccess,
                     expr.Equation(
                       extract_expr(g, node),
                       extract_expr(g, target)))
    elif nv < tv:
        return Build(TooLow, node)
    else:
        return Build(TooHigh, node)

def start_fixer_seq(g, badnode: NRef):
    if not badnode:
        return
    g.undo_consumption(badnode)
    seqnode = make_action_sequence(
        g,
        ExcludeOperand(),
        Reglom(),
        AddAllInGlom(),
        member_of=g.ws
    )
    seqnode.min_activation = 10.0 # HACK

##### Custom Actions

@dataclass
class ActivateSlipnode(Action):
    slipnode: int

    def go(self, g):
        ws = g.ws
        new_node = g.as_node(g.copy_group(self.slipnode, ws))
        #print('ASLIP', self.slipnode, g.nodestr(new_node))
        new_node.min_activation = 10.0 # HACK
        #g.set_activation(new_node, 10.0)
        g.deactivate(self.slipnode)
        g.as_node(self.slipnode).dont_activate_slipnode = True   # HACK

        new_node.on_build()  # HACK for ActionSeqNode 

@dataclass
class SeekAndGlom(Action):
    criteria: Union[Criterion, List[Criterion], None]
    within: Union[int, None]=None

    threshold = 1.0

    def go(self, g):
        glommees = g.find_all(*as_iter(self.criteria), within=self.within)
        if glommees:
            #g.do(Build.maybe_make(g, Glom, [glommees], {}))
            g.do(Build.maybe_make(g, Glom, glommees))
            g.new_state(self.actor, Completed)
        # TODO else: FAILED

@dataclass
class NoticeAllSameValue(Action):
    value: Any=None
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
        if self.value is None:
            raise NeedArg(self, 'value')
        if all(
            g.value_of(memberid) == self.value
                #for memberid in g.members_of(self.within)
                for memberid in g.find_all(OfClass(Number), within=self.within)
        ):
            #g.do(Build.maybe_make(g, AllMembersSameValue, [self.within], {}))
            g.do(Build.maybe_make(g, AllMembersSameValue, self.within))
            g.new_state(self.actor, Completed)
        else:
            raise NotAllSameValue(self, value=self.value, within=self.within)
        # TODO else: FAILED

@dataclass
class CountMembers(Action):
    within: Union[int, None]=None

    threshold = 1.0

    def go(self, g):
        if not self.within:
            raise NeedArg(self, 'within')
        num_members = len(g.neighbors(self.within, port_label='members'))
        g.add_node(Count, taggees=self.within, value=num_members)
        g.new_state(self.actor, Completed)

@dataclass
class NoticeSameValue(Action):
    node1: Union[int, None]=None
    node2: Union[int, None]=None

    threshold: float = 1.0

    def go(self, g):
        if not self.node1:
            raise NeedArg(self, 'node1')
        if not self.node2:
            raise NeedArg(self, 'node2')
        if g.value_of(self.node1) == g.value_of(self.node2):
            g.do(
#                Build.maybe_make(
#                    g, SameValue, [], dict(taggees=[self.node1, self.node2])
#                )
                Build.maybe_make(
                    g, SameValue, taggees=[self.node1, self.node2]
                )
            )
            g.new_state(self.actor, Completed)
        else:
            raise NotSameValue(self, self.node1, self.node2)

@dataclass
class AddAllInGlom(Action):
    within: Union[int, None]=None

    threshold: float = 1.0

    def go(self, g):
        if not self.within:
            raise NeedArg(self, 'within')
        g.add_node(Proposal,
            ConsumeOperands(),
            consume_operands=g.find_all(
                OfClass(Number), CTagged(Avail),
                within=self.within
            ),
            proposed_operator=g.look_for(
                OfClass(Plus), CTagged(Allowed),
                within=g.ws
            ),
            member_of=g.ws
        )
        g.new_state(self.actor, Completed)

@dataclass
class ConsumeOperands(Action):
    consume_operands: Union[NRefs, None]=None
    proposed_operator: Union[NRef, None]=None

    def go(self, g):
        g.consume_operands(self.consume_operands, self.proposed_operator)
        g.new_state(self.actor, Completed)

@dataclass
class SeekArg(Action):
    for_node: NRef
    port_label: PortLabel
    nodeclass: Type[Node]
    
    def go(self, g):
        found_node = g.look_for(OfClass(self.nodeclass))
        if found_node:
            g.add_override_node(self.for_node, self.port_label, found_node)
            g.boost_activation(self.for_node)
            g.new_state(self.actor, Completed)
            g.remove_node(g.neighbors(self.actor, 'rm_on_success'))

@dataclass
class SeekNewValue(Action):
    for_node: NRef
    port_label: PortLabel
    within: NRef

    def go(self, g):
        found_node = g.look_for(HasAttr('value'), within=self.within)
        if found_node:
            g.add_override_node(self.for_node, self.port_label, found_node)
            g.boost_activation(self.for_node)
            g.new_state(self.actor, Completed)
            g.remove_node(g.neighbors(self.actor, 'rm_on_success'))
        else:
            print('SeekNewValue FAILED')  #DEBUG
            #TODO raise an exception

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

@dataclass
class ExcludeOperand(Action):
    within: Union[NRef, None]=None

    threshold: float = 1.0

    def go(self, g):
        if not self.within:
            raise NeedArg(self, 'within')
        operand = g.look_for(OfClass(Brick), within=self.within)
        if operand:
            g.add_node(Exclude, operand)
            g.new_state(self.actor, Completed)
        else:
            print('ExcludeOperand FAILED')  #DEBUG

@dataclass
class Reglom(Action):
    glom: Union[NRef, None]=None

    threshold: float = 1.0

    def go(self, g):
        if not self.glom:
            raise NeedArg(self, 'glom')
        #old_members = as_set(g.members_of(self.glom))
        old_members = as_set(g.find_all(
            CTagged(Avail),
            subset=g.members_of(self.glom)
        ))
        excluded = as_set(g.find_all(CTagged(Exclude), subset=old_members))
        new_members = old_members - excluded
        g.remove_node(self.glom)
        if new_members:
            g.add_node(Glom, new_members)
            g.new_state(self.actor, Completed)
        else: # should signal failure  TODO
            print('REGLOM FAILED', self.glom, old_members, excluded, new_members)



##### Nodeclasses defined in Python

is_number = OfClass(Number)

class Slipnet(Group, ActiveNode):

    min_support_for = 1.0
    min_activation = 1.1

    def actions(self, g):
        actives = g.find_all(
            Activated(), OfClass(ActiveNode),
            subset=g.members_of(self)
            #TODO within=self ?
        )
        #print('ACTIVE SLIPNODES', actives)
        return [
            ActivateSlipnode(slipnode)
                for slipnode in actives
                    if not g.as_node(slipnode).dont_activate_slipnode
        ]

class AssessorScout(ActiveNode):
    node_params = NodeParams(MateParam('target', 'tags'))

    def actions(self, g):
        node = g.look_for(CTagged(Avail), NotTagged(Assessment), within=g.ws)
        if node:
            return assess(g, node, g.neighbor(self, 'target'))

class FixerScout(ActiveNode):

    min_activation = 1.0

    def actions(self, g):
        badnode = g.look_for(
            OfClass(Block), CTagged(Avail), CTagged(NotGoodEnough),
            within=g.ws
        )
        if badnode:
            return FuncAction(start_fixer_seq, badnode)

class Proposal(ActiveNode):
    node_params = NodeParams(AttrParam('action'))
    # We expect more arguments, which we will pass to 'action'.

    is_duplicable = True  # HACK  Is already_built mis-rejecting this?

    def actions(self, g):
        return self.action.with_overrides_from(g, self)
    
class AllBricksAvail(Tag, ActiveNode):

    initial_support_for = 1.0
    initial_activation = 1.0 #10.0  # HACK

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
            return Build.maybe_make(g, AllBricksAvail, taggees=bricks)
            self.g.deactivate(self)
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
        #return Build.maybe_make(g, Glom, [all_with_same_value], {})
        return Build.maybe_make(g, Glom, all_with_same_value)

class MemberCounter(ActiveNode):

    def actions(self, g):
        group_node = g.look_for(OfClass(Group))
        if group_node is None:
            return
        num_members = len(g.neighbors(group_node, port_label='members'))
        # TODO make_build3 -> Build.maybe_make
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
        # TODO make_build3 -> Build.maybe_make
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
        operand=Brick,
        glom=Glom,
    )

    def actions(self, g):
        # TODO Inheritance in ActionFailure
        if isinstance(self.reason, NeedArg):
            action = SeekArg(
                g.neighbor(self, 'taggees'),
                self.reason.name,  # HACK: assumes NeedArg
                self.port_label_to_nodeclass[self.reason.name]  # HACK
            )
        elif isinstance(self.reason, NotAllSameValue):
            action = SeekNewValue(
                for_node=g.neighbor(self, 'taggees'),
                port_label='value',
                within=self.reason.within
            )
        else:
            return  # No recognized reason => no action

        return [
            StartScout(
                action=action,
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
        self.nodeclasses['Failed'] = Failed
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
        #self.add_node(SuccessScout, target=targetid, min_support_for=1.0)
        self.add_node(NoticeAllBricksAvail, member_of=ws)
        self.add_node(FixerScout, member_of=ws)

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

    def undo_consumption(self, nref: MaybeNRef):
        if not nref or not self.has_tag(nref, Avail):
            return
        self.remove_tag(nref, Avail)
        source = g.neighbor(nref, 'source')
        operands = g.neighbors(source, 'operands')
        self.remove_tag(operands, Consumed)
        for operand in operands:
            self.add_tag(Avail, operand)


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

def newg(numble=Numble([2, 2, 2, 2, 2], 10)):
    global g, ws
    #numble = Numble([1, 1, 1, 1, 1], 5)
    #numble = Numble([2, 2, 2, 2, 2], 10)
    g = new_graph(numble, seed=8028868705202140491)
    ws = g.ws
    return g

def p():
    '''Print just the four nodes of interest for the demo.'''
    pg(g, [20, 21, 22, 23, 24])

if __name__ == '__main__':
    ShowAnnotations.start_logging()
    ShowActiveNodes.start_logging()
    ShowActionList.start_logging()
    #ShowActionsChosen.start_logging()
    ShowActionsPerformed.start_logging()
    ShowPrimitives.start_logging()
    #ShowIsMatch.start_logging()

    newg(Numble([1, 1, 1, 1, 1], 4))

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

    #g.do_timestep(num=1)
    #pg(g)

    #print("\nMANUAL ACTION HERE: activating slipnode for 'Notice that all the bricks are 1, count them up, and notice that the count equals the target, and add up the bricks.\n")
    #g.copy_group(8, 1)  # HACK to activate ActionSeqNode from slipnet
    #g.do_timestep(num=29)
    #pg(g)


    #dt = g.datum(3)
    #dt2 = copy(dt)
    #kwargs = {'action': SeekAndGlom(criteria=OfClass(Brick), within=None), 'state': Start}
    #an = ActionNode(**kwargs)

    #g.do_timestep(num=55)
    #pdb.run('g.do_timestep()')
