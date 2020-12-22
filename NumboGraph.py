# NumboGraph.py -- Graph class and associated nodeclasses, Acs, Actions, and
#                  Criteria for Numbo experiments

from dataclasses import dataclass, field
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Callable
from operator import add, mul
from functools import reduce

from Node import Node, NRef, NRefs, CRef, MaybeNRef, MaybeCRef, PortLabel, \
    NodeId
from PortMates import PortMates
from NodeParams import NodeParams, AttrParam, MateParam
from StdGraph import Graph, MyContext, InWorkspace, pg
from codegen import make_python, compile_fargish
from Numble import make_numble_class, prompt_for_numble
from Ac import Ac, AcNode, AdHocAcNode, All, AllAre, TagWith, AddNode, OrFail, \
    MembersOf, Len, EqualValue, Taggees, LookFor, Raise, PrintEnv, AcNot, \
    SelfDestruct, FindParamName, LookForArg, AddOverride, RemoveBlockedTag, \
    WithNameOverride, LookForTup, HasKwargs, Persistent, Boost, OrBlock
from Ac import CantFind, NotEqualValue, AsgnNeighbors
from ActiveNode import ActiveNode, Start, Completed, HasUpdate, \
    make_action_sequence
from Action import Action, Actions, BuildAgent
from criteria import OfClass, Tagged as CTagged, HasThisValue, And, \
    NotTheArgsOf, Criterion, MinActivation
from exc import FargDone, NeedArg, FizzleAndFail, FizzleAndBlock
from util import Quote, omit, first


prog = '''
tags -- taggees
within -- overriding
node1 -- overriding
node2 -- overriding
target -- overriding
operands -- consumer
proposed_operands -- proposer
proposed_operator -- proposer
result_consumer -- source  # HACK: should be 'consumer'; see unique_mate().
minuend -- consumerM  # HACK TODO Fix: violates unique mate for 'consumer'
subtrahend -- consumerS

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

##### Hacks

Number.is_duplicable = True
Operator.is_duplicable = True

Want.min_activation = 1.0

def failed_display_name(self: Failed) -> str:
    return f'{self.__class__.__name__}({self.reason.__class__.__name__})'

Failed.display_name = failed_display_name
    

# TODO rm these functions?

#def plus_result(self, g: 'G', node: NRef) -> int:
def plus_result(self: Plus) -> int:
    # TODO Appropriate exception(s) if an operand is missing a value or it's
    # the wrong type or there aren't enough operands.
    operands = self.g.neighbors(self, 'operands')
    return sum(self.g.value_of(o) for o in operands)
Plus.result_value = plus_result

def times_result(self: Times) -> int:
    # TODO Appropriate exception(s) if an operand is missing a value or it's
    # the wrong type or there aren't enough operands.
    operands = self.g.neighbors(self, 'operands')
    return reduce(mul, (self.g.value_of(o) for o in operands), 1)
Times.result_value = times_result

def minus_result(self: Minus) -> int:
    # TODO Appropriate exception(s) if an operand is missing a value or it's
    # the wrong type or there aren't enough operands.
    return (
        self.g.value_of(self.g.neighbor(self, 'minuend'))
        -
        self.g.value_of(self.g.neighbor(self, 'subtrahend'))
    )
Minus.result_value = minus_result

# Custom exceptions

@dataclass
class NumboSuccess(FargDone):
    succeeded: bool = field(init=False, default=True)
    node: NRef
    target: NRef

@dataclass
#class NotAllThisValue(AcFailed):
class NotAllThisValue(FizzleAndFail):
    #TODO Supply the commented-out parameters.
    value: Any=None
    #within: NRef=None

    @classmethod
    def from_env(cls, value: Union[int, None]=None, **kwargs):
        def ctor(g, ac, actor, env) -> FizzleAndFail:
            return NotAllThisValue(value=value)
        return ctor

# Custom functions

# TODO rm?
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
class OperatorWithAvailOperands(Criterion):

    def __call__(self, g, nref):
        node = g.datum(nref)
        if not g.is_of_class(node, Operator):
            return False
        operands = g.neighbors(node, 'operands')
        if len(operands) < 2:
            return False
        for operand in operands:
            if not g.has_tag(operand, Avail):
                return False
        return True

@dataclass
class OLDConsumeOperands(Action):
    consume_operands: Union[NRefs, None]=None
    proposed_operator: Union[NRef, None]=None

    def go(self, g, actor):
        g.consume_operands(
            self.consume_operands,
            self.proposed_operator,
            actor=actor
        )
        g.new_state(actor, Completed)

@dataclass
class ConsumeOperands(Action):
    '''Calls g.consume_operands(). Arguments must be installed in this object
    before .go() is called. The Node that owns this Action should call
    .with_overrides_from() on this Action before running it. One of those
    filled-in arguments must be 'operator'.'''
    # TODO That comment shows that ConsumeOperands is pretty ugly and should
    # be redesigned.

#    operator: CRef

    def as_kwargs(self):
        return omit(self.__dict__, ['actor', 'operator'])

    def go(self, g, actor):
        actor = g.datum(actor)
        # TODO Exception if actor does not exist?
        #print('CONS', self.as_kwargs())
        g.consume_operands(self.operator, actor, **self.as_kwargs())
        g.new_state(actor, Completed)

@dataclass
class CountMembers(Action):
    within: Union[int, None]=None

    threshold = 1.0

    def go(self, g, actor):
        if not self.within:
            raise NeedArg(ac=self, name='within')
        num_members = len(g.neighbors(self.within, port_label='members'))
        g.add_node(Count, taggees=self.within, value=num_members)
        g.new_state(self.actor, Completed)

# Custom Acs

@dataclass
class BuildOpResult(HasKwargs, Ac):
    opclass: MaybeCRef = None
    # and kwargs, filled in by HasKwargs.__init__

    def __init__(self, opclass: MaybeCRef=None, **kwargs):
        self.opclass = opclass
        super().__init__(self, **kwargs)

    def go(self, g, actor, env):
        opclass = self.get(g, actor, env, 'opclass')
        kwargs = self.get_kwargs(g, actor, env)
        (operator, result) = g.build_op_and_result(opclass, actor, **kwargs)
        env['operator'] = operator
        env['result'] = result

# Custom nodeclasses

class NoticeSolved(AcNode):
    acs = [
        LookFor(OfClass(Target), asgn_to='target'),
        LookFor(CTagged(Avail), cond=EqualValue('node', 'target')),
        Raise(NumboSuccess, node='node', target='target')
    ]

class AllBricksAvail(Tag, HasUpdate, ActiveNode):
    update_action: Actions = Ac.as_action([
        All(OfClass(Brick), within=MyContext),
        AcNot(AllAre(CTagged(Avail))),
        SelfDestruct()
    ])

    def actions(self):
        pass

class NoticeAllBricksAreAvail(Persistent, AcNode):
    acs = [
        All(OfClass(Brick)),  # missing 'within' argument
        AllAre(CTagged(Avail)),
        TagWith(AllBricksAvail, taggees='nodes')
    ]

class SeekAndGlom(AcNode):
    node_params = NodeParams(
        AttrParam('seekclass', Brick)  # Specific to testNumboClasses: default
    )                                  # to seeking Brick nodes

    threshold = 1.0
    acs = [
        All(OfClass('seekclass'), within=MyContext),
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
    threshold = 1.0
    acs = [
        All(OfClass(Number)),
        OrFail(
            AllAre(HasThisValue(value=3)),
            NotAllThisValue.from_env(value=3)
            # TODO Get 'value' from env so search and exc are assuredly
            # consistent
        ),
        TagWith(AllMembersHaveThisValue, taggees='within')
    ]

class NoticeCountSameAsTarget(AcNode):
    threshold = 1.0
    acs = [
        OrFail(
            LookFor(OfClass(Count), asgn_to='node1'),
            CantFind.from_env(criteria=OfClass(Count))
        ),
        OrFail(
            LookFor(OfClass(Target), asgn_to='node2'),
            CantFind.from_env(criteria=OfClass(Target))
        ),
        OrFail(
            EqualValue(),
            NotEqualValue.from_env(node1='node1', node2='node2')
        ),
        Taggees('node1', 'node2'),
        TagWith(SameValue)
    ]

class Proposal(ActiveNode):
    node_params = NodeParams(AttrParam('action'))
    # We expect more arguments, which we will pass to 'action'.

    is_duplicable = True  # HACK  Is already_built mis-rejecting this?

    def proposed_kwargs(self) -> Dict[PortLabel, NodeId]:
        '''Strips leading 'proposed_' from port labels and returns a
        dictionary mapping the resulting names to this node's neighbors at
        the corresponding ports.'''
        result = {}
        for pk in self.g.port_labels_of(self):
            if not pk.startswith('proposed_'):
                continue
            k = pk[9:]
            v = self.g.neighbors(self, port_label=pk)
            if not v:
                v = None
            elif len(v) == 1:
                v = first(v)
            result[k] = v
        return result

    def actions(self):
        #return self.action.with_overrides_from(self.g, self)
        return self.action.with_overrides_from(self.g, self.proposed_kwargs())

class AddAllInGlom(AcNode):
    threshold = 1.0
    acs = [
        All(And(OfClass(Number), CTagged(Avail))),
        WithNameOverride(
            LookFor(And(OfClass(Plus), CTagged(Allowed)), within=InWorkspace),
            within='opwithin'
        ),
        AddNode(
            Proposal,
            action=ConsumeOperands(),
            proposed_operands='nodes',
            proposed_operator='node',
        )
    ]

class NumboGraph(Graph):
    def __init__(self, numble, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.nodeclasses.update(nodeclasses)
        self.add_nodeclasses(
            AllBricksAvail, NoticeAllBricksAreAvail, FillParamScout, Proposal
        )
        self.port_mates += port_mates
        self.numble = numble
        self.make_initial_nodes()

    def make_initial_nodes(self):
        self.add_node(Workspace)
        self.make_slipnet()
        self.numble.build(self, self.ws)
        self.add_node(NoticeSolved, member_of=self.ws, within=self.ws)

    def make_slipnet(self):
        self.seqnode = make_action_sequence(
            self,
            SeekAndGlom(within=self.ws, criteria=OfClass(Brick)),
            NoticeAllHaveThisValue(value=1, within=None),
            CountMembers(within=None),
            NoticeCountSameAsTarget(
                node1=None, node2=None, value=1, within=InWorkspace
            ),
            AddAllInGlom(),
            member_of=self.slipnet,
        )

    def build_op_and_result(self, operator_class: CRef, actor=None, **kwargs) \
    -> Tuple[NRef, NRef]:
        '''Builds operator node, linked via port_labels to existing nodes
        as provided in kwargs. Builds result Block with value calculated
        by operator_class.result_value(). Returns the new nodes in a tuple:
        (operator, result).'''
        # TODO Raise an exception if missing or improper operands?
        if 'member_of' not in kwargs:
            kwargs['member_of'] = self.containers_of(actor)
        operator = self.add_node(operator_class, builder=actor, **kwargs)
        result_value = self.call_method(operator, 'result_value')
        result = self.add_node(
            Block, value=result_value, source=operator, builder=actor
        )
        return (operator, result)
        
    def consume_operands(self, operator_class: CRef, actor=None, **kwargs) \
    -> Tuple[NRef, NRef]:
        '''kwargs is port_label=NRefs for each operand.'''
        operands = kwargs.values() # Wrong: member_of is not an operand  TODO 
        if not self.has_tag(operands, Avail):
            return  # TODO Raise a failure exception.
        (operator, result) = self.build_op_and_result(
            operator_class, actor, **kwargs
        )
        self.move_tag(Avail, operands, result)
        #self.add_tag(Consumed, operands)
        self.add_tag(Done, actor) # TODO rm?
        return (operator, result)

def newg(numble=Numble([4, 5, 6], 15), seed=8028868705202140491):
    return NumboGraph(numble=numble, seed=seed)

if __name__ == '__main__':
    g = newg()
    pg(g)


