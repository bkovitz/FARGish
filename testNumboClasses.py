# testNumboClasses.py -- Common nodeclasses for unit tests and acceptance tests

from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Callable
from operator import add, mul
from functools import reduce

from Node import Node, NRef, NRefs, CRef, MaybeNRef
from PortMates import PortMates
from NodeParams import NodeParams, AttrParam, MateParam
from StdGraph import Graph, MyContext, pg
from codegen import make_python, compile_fargish
from Numble import make_numble_class, prompt_for_numble
from Ac import Ac, AcNode, AdHocAcNode, All, AllAre, TagWith, AddNode, OrFail, \
    MembersOf, Len, EqualValue, Taggees, LookFor, Raise, PrintEnv, AcNot, \
    SelfDestruct, FindParamName, LookForArg, AddOverride, RemoveBlockedTag
from ActiveNode import ActiveNode, Start, Completed, HasUpdate, \
    make_action_sequence
from Action import Action, Actions, BuildAgent
from criteria import OfClass, Tagged as CTagged, HasThisValue
from exc import AcNeedArg, ActionFailure, AcFailed, FargDone, NeedArg


'''
port_mates = PortMates([('taggees', 'tags'), ('target', 'tags')])

class Workspace(Node):
    pass
class Number(Node):
    node_params = NodeParams(AttrParam('value'))
class Brick(Number):
    is_duplicable = True
class Target(Number):
    pass
class Block(Number):
    pass
class Tag(Node):
    node_params = NodeParams(MateParam('taggees', 'tags'))
class Avail(Tag):
    pass
class Allowed(Tag):
    pass
class Want(Node):
    node_params = NodeParams(MateParam('target', 'tags'))
class Operator(Node):
    is_duplicable = True
class Plus(Operator):
    pass
class Times(Operator):
    pass
'''

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
Want : Tag
Avail, Consumed, Allowed, Done : Tag
SameValue, AllMembersHaveThisValue : Tag
Blocked(reason) : Tag
Failed(reason): Tag
Count(value) : Tag

Number(value)
Brick, Target, Block(source, consumer) : Number

Operator(operands, consumer)
Plus, Times : Operator
Minus(minuend, subtrahend) : Operator

Group(members)
Glom : Group
'''
exec(compile_fargish(prog), globals())

Numble = make_numble_class(
    Brick, Target, Want, Avail, Allowed, [Plus, Times, Minus]
)

# Custom exceptions

@dataclass
class NumboSuccess(FargDone):
    node: NRef
    target: NRef

@dataclass
class NotAllThisValue(AcFailed):
    #value: Any=None
    #within: NRef=None
    ac: Ac
    actor: MaybeNRef

# Custom functions

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

# Custom Actions

@dataclass
class ConsumeOperands(Action):
    consume_operands: Union[NRefs, None]=None
    proposed_operator: Union[NRef, None]=None

    def go(self, g, actor):
        g.consume_operands(self.consume_operands, self.proposed_operator)
        g.new_state(self.actor, Completed)

@dataclass
class CountMembers(Action):
    within: Union[int, None]=None

    threshold = 1.0

    def go(self, g, actor):
        if not self.within:
            raise NeedArg(self, 'within')
        num_members = len(g.neighbors(self.within, port_label='members'))
        g.add_node(Count, taggees=self.within, value=num_members)
        g.new_state(self.actor, Completed)

# Custom nodeclasses

class AllBricksAvail(Tag, HasUpdate, ActiveNode):

    update_action: Actions = Ac.as_action([
        All(OfClass(Brick), within=MyContext),
        AcNot(AllAre(CTagged(Avail))),
        SelfDestruct()
    ])

    def actions(self):
        pass

class NoticeAllBricksAreAvail(AcNode):
    acs = [
        All(OfClass(Brick)),  # missing 'within' argument
        AllAre(CTagged(Avail)),
        TagWith(AllBricksAvail, taggees='nodes')
    ]

class SeekAndGlom(AcNode):
    acs = [
        All(OfClass(Brick)),
        AddNode(Glom, members='nodes')
    ]

class FillParamScout(AcNode):
    node_params = NodeParams(
        MateParam('behalf_of', 'agents'),
        MateParam('problem', 'general')
    )

    acs = [
        # name <- problem.reason.name
        # add_override(behalf_of.name, look_for_arg(name))
        FindParamName(),
        LookForArg(),
        AddOverride(),
        RemoveBlockedTag()
    ]

class NoticeAllHaveThisValue(AcNode):
    acs = [
        All(OfClass(Number)),
        OrFail(
            AllAre(HasThisValue(value=3)),
            NotAllThisValue
        ),
        TagWith(AllMembersHaveThisValue, taggees='within')
    ]

class NoticeSameValue(AcNode):
    acs = [
        EqualValue('node1', 'node2'),
        Taggees('node1', 'node2'),
        TagWith(SameValue)
    ]

class Proposal(ActiveNode):
    node_params = NodeParams(AttrParam('action'))
    # We expect more arguments, which we will pass to 'action'.

    is_duplicable = True  # HACK  Is already_built mis-rejecting this?

    def actions(self):
        return self.action.with_overrides_from(self.g, self)

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

class NumboTestGraph(Graph):
    def __init__(self, numble, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.nodeclasses.update(nodeclasses)
        self.add_nodeclasses(
            AllBricksAvail, NoticeAllBricksAreAvail, FillParamScout, Proposal
        )
        self.port_mates += port_mates

        # Make initial nodes
        self.add_node(Workspace)
        self.make_slipnet()
        numble.build(self, self.ws)

    def make_slipnet(self):
        self.seqnode = make_action_sequence(
            self,
            SeekAndGlom(within=self.ws, criteria=OfClass(Brick)),
            NoticeAllHaveThisValue(value=1, within=None),
            CountMembers(within=None),
            NoticeSameValue(node1=None, node2=None),
            AddAllInGlom(),
            member_of=self.slipnet,
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
        )
        result_id = self.add_node(
            Block,
            value=arith_result0(self, operator_class, operand_ids),
            source=operator_id,
        )
        self.move_tag(Avail, operand_ids, result_id)
        self.add_tag(Consumed, operand_ids)
        self.add_tag(Done, actor)

def newg(numble=Numble([4, 5, 6], 15), seed=8028868705202140491):
    return NumboTestGraph(numble=numble, seed=seed)

if __name__ == '__main__':
    g = newg()
    pg(g)
