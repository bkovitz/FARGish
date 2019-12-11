# numbospec.py -- Data structures that specify the Numbo model

from FARGSpec import FARGSpec, EdgeInfo
from PortGraph import Node, Tag, NodesWithSalience, pg, ps
from bases import ActiveNode, Action
from criteria import Tagged, HasValue
from exc import NumboSuccess
from util import as_iter, nice_object_repr, intersection
import expr

from abc import ABC, abstractmethod
from random import choice
from itertools import chain

edgeinfos = [
    EdgeInfo('taggees', 'tags', clas='Tag'),
    EdgeInfo('support_from', 'support_to', clas='Support'),
    EdgeInfo('members', 'member_of', clas='Member'),
    EdgeInfo('viewing', 'view', clas='View'),
    EdgeInfo('agent', 'agent_for', clas='Agent'),
    EdgeInfo('consumer', 'source', clas='Source'),
]

spec = FARGSpec(edgeinfos)

# Node classes other than tags and scouts

class Workspace(Node):
    min_support_for = 0.01  # Prevents node from being removed

class Number(Node):

    def __init__(self, n):
        self.value = n

    def display_name(self, g, node):
        return self.datumstr(g, node)

    def is_attrs_match(self, other):
        try:
            return self.value == other.value
        except AttributeError:
            return False

    def expr(self, g, node):
        return expr.Number(g.value_of(node))

class Brick(Number):
    default_salience = 0.1
    min_support_for = 0.1
    #gives_reciprocal_support = False

class Target(Number):
    needs_source = True
    default_salience = 1.0
    min_support_for = 1.0

class Block(Number):
    needs_source = True

    def expr(self, g, node):
        source = g.neighbor(node, port_label='source')
        #TODO What if there's more than one source? Or none?
        return g.expr(source)

class Operator(Node, ABC):

    needs_source = True

    def symbol(self):
        return '?'

    @abstractmethod
    def result_value(self, g, node, operands=None):
        pass

    @classmethod
    def failed_with_these_operands(cls, g, operands):
        former_consumerss = []
        for operand in operands:
            former_consumerss.append(
                set(neighbor for neighbor in g.neighbors(
                                 operand, 'former_consumer'
                             )
                                 if g.is_of_class(neighbor, cls)
                )
            )
        print('SET', former_consumerss, set.intersection(*former_consumerss))
        return len(set.intersection(*former_consumerss)) > 0

    def operands(self, g, node):
        return list(g.neighbors(node, 'source'))

    def operand_exprs(self, g, node):
        return [
            g.expr(operand)
                for operand in self.operands(g, node)
        ]

    def expr(self, g, node):
        return self.expr_class(*self.operand_exprs(g, node))

    def fail(self, g, node):
        TagWith(Failed, taggee=node).go(g) #TODO add_tag
        g.remove_tag(node, Avail)
        for source in g.neighbors(node, 'source'):
            g.remove_edge(node, 'source', source, 'consumer')
            g.add_edge(node, 'former_source', source, 'former_consumer')
            g.cascade_fail(source)

    cascade_fail = fail

    #TODO rm
    def expr_str(self, g, node):
        sources = g.neighbors(node, 'source')
        sep = ' ' + self.symbol() + ' '
        return sep.join(g.datum(source).expr_str(g, source)
                           for source in sources)

class Plus (Operator):

    expr_class = expr.Plus

    @classmethod
    def calculate(cls, operands):
        return sum(operands)

    def symbol(self):
        return '+'

    def result_value(self, g, node, operands=None):
        #TODO OAOO
        operands = list(g.neighbors(node, 'source'))
        if operands:
            #TODO OAOO calculate
            return sum(g.value_of(operand) for operand in operands)
        else:
            return None

    def update_support(self, g, this_node):
        for could_make in g.neighbors(this_node, port_label='could_make'):
            if any(
                g.value_of(o) == 0
                    for o in g.neighbors(could_make, port_label='operands')
            ):
                g.oppose(this_node, could_make)
            else:
                g.add_support(this_node, could_make)

class Minus (Operator):

    expr_class = expr.Minus

    def symbol(self):
        return '-'

class Times (Operator):

    expr_class = expr.Times

    @classmethod
    def calculate(cls, operands):
        v = 1
        for operand in operands:
            v *= operand
        return v

    def symbol(self):
        return '*'

    def result_value(self, g, node):
        operands = list(g.neighbors(node, 'source'))
        if operands:
            #TODO OAOO calculate
            v = 1
            for operand in operands:
                v *= g.value_of(operand)
            return v
        else:
            return None

    def update_support(self, g, this_node):
        for could_make in g.neighbors(this_node, port_label='could_make'):
            if any(
                g.value_of(o) == 1
                    for o in g.neighbors(could_make, port_label='operands')
            ):
                g.oppose(this_node, could_make)
            else:
                g.add_support(this_node, could_make)

class Div (Operator):

    expr_class = expr.Div

    def symbol(self):
        return '/'
        
Operator.operator_classes = {'+': Plus, '-': Minus, '*': Times, '/': Div}
all_operator_classes = frozenset([Plus, Times])
    #TODO Minus: need to represent the order of
    #the operands in the graph.

# Tag classes and ancillary functions

class Avail(Tag):
    pass

class Failed(Tag):
    pass

class Consumed(Tag):
    pass

class WantBuiltFromBricks(Tag, ActiveNode):
    min_support_for = 1.0

    def actions(self, g, thisid):
        wantedid = g.taggee_of(thisid)
        wanted_number = g.value_of(wantedid)
        foundid = g.look_for(Tagged(Avail), HasValue(wanted_number))
        if is_built_from_bricks(g, foundid):
            responses = [HaltNumbo(NumboSuccess(g, foundid))]
        else:
            # Make an OperandsScout if we don't already have one?
            # How do we tell when there are no more Avail operands, and
            # what do we do then?
            #wanted_number = g.taggee_value(thisid)
            responses = [
                Build.maybe_make(
                    g, thisid, 'agents', OperandsScout, 'agent_for'
                )
            ]
        return responses

def is_built_from_bricks(g, nodeid):
    # True iff nodeid is a Brick or all its 'source' neighbors trace all the
    # way to Bricks.
    if not g.has_node(nodeid):
        return False
    datum = g.datum(nodeid)
    if datum.needs_source:
        sources = g.neighbors(nodeid, port_label='source')
        if sources:
            return all(is_built_from_bricks(g, s) for s in sources)
        else:
            return False
    else:
        return True

# Scout classes

class OperandsScout(ActiveNode):

    def actions(self, g, thisid):
        '''Chooses two Avail numbers and makes a ConsumeOperands using them
        as operands if possible.'''
        operandids = NodesWithSalience(g, g.nodes_with_tag(Avail))
        if len(operandids) >= 2:
            actions = [ConsumeOperands.make(g, operandids.choose(k=2))]
            # NEXT if can't make ConsumeOperands, keep choosing
        elif len(operandids) == 1:
            actions = [FailResult(operandids.choose1())]
        else:
            actions = []  # TODO GiveUp
        return [ActionSequence(a, Apoptosis(thisid)) for a in actions]
        

# Actions

class HaltNumbo(Action):

    def __init__(self, done_object):
        self.done_object = done_object

    def go(self, g):
        g.set_done(self.done_object)

class ActionSequence(Action):

    def __init__(self, *actions):
        '''Each action can be an Action or an iterable of Actions.'''
        self.actions = actions

    def go(self, g):
        for action_or_actions in self.actions:
            for action in as_iter(action_or_actions):
                action.go(g)

class Apoptosis(Action):

    def __init__(self, nodeid):
        self.nodeid = nodeid

    def go(self, g):
        g.remove_node(self.nodeid)
            
class Build(Action):

    @classmethod
    def maybe_make(
        cls, g, nodeid, from_port_label, new_node_class, to_port_label
    ):
        '''Returns a Build action to make a link a node of new_node_class
        to nodeid via the given ports if such a node does not already
        exist. Otherwise returns None.'''
        if cls.is_already_built(
            g, nodeid, from_port_label, new_node_class, to_port_label
        ):
            return None
        else:
            return Build(nodeid, from_port_label, new_node_class, to_port_label)

    @classmethod
    def is_already_built(
        cls, g, nodeid, from_port_label, new_node_class, to_port_label
    ):
        return any(
            g.is_of_class(n, new_node_class)
                for n in g.neighbors(nodeid, from_port_label)
        )

    def __init__(self, nodeid, from_port_label, new_node_class, to_port_label):
        self.nodeid = nodeid
        self.from_port_label = from_port_label
        self.new_node_class = new_node_class
        self.to_port_label = to_port_label

    def go(self, g):
        if not self.is_already_built(
            g,
            self.nodeid,
            self.from_port_label,
            self.new_node_class,
            self.to_port_label
        ):
            new_node = g.make_node(
                self.new_node_class,
                g.member_of(self.nodeid)
            )
            g.add_edge(
                self.nodeid,
                self.from_port_label,
                new_node,
                self.to_port_label
            )

class ConsumeOperands(Action):

    @classmethod
    def make(cls, g, operandids):
        '''If any Operators remain that haven't been tried on the operandids
        (node ids), returns a ConsumeOperands Action to build a full
        arithmetic operation on the operandids, linking them to a new
        Operator, whose class is chosen randomly, which links to a result
        Block. Otherwise returns None.'''
        operandids = list(operandids)
        consumers = intersection(
            *[g.neighbors(o, port_label='consumer') for o in operandids]
        )
        consumer_classes = set([g.class_of(c) for c in consumers])
        untried_classes = all_operator_classes.difference(consumer_classes)
        #print('MAKE', consumers, consumer_classes, untried_classes)
        if untried_classes:
            return ConsumeOperands(operandids, choice(list(untried_classes)))
        else:
            return None

    def __init__(self, operandids, operator_class):
        self.operandids = operandids
        self.operator_class = operator_class

    def go(self, g):
        # If operandids not already linked to a common self.operator_class,
        # build operator, result, and links.
        #TODO Check if operator is already built and linked
        if self.is_already_consumed(g):
            return
        container = g.common_container(self.operandids)
        operatorid = g.make_node(self.operator_class, container=container)
        for operandid in self.operandids:
            g.add_edge(operatorid, 'source', operandid, 'consumer')
        result_value = self.operator_class.calculate(
            [g.value_of(o) for o in self.operandids]
        )
        resultid = g.make_node(Block(result_value), container)
        g.add_edge(resultid, 'source', operatorid, 'consumer')
        for operatorid in self.operandids:
            g.add_tag(Consumed, operatorid)
            g.remove_tag(operatorid, Avail)
        g.add_tag(Avail, resultid)

    def is_already_consumed(self, g):
        consumers = intersection(
            *[g.neighbors(o, port_label='consumer') for o in self.operandids]
        )
        consumer_classes = set([g.class_of(c) for c in consumers])
        return self.operator_class in consumer_classes

class FailResult(Action):
    '''Marks a number tagged Avail as Failed, along with its source, if any.
    Moves the Avail tag to the source's operands and removes their Consumed
    tags.'''

    def __init__(self, resultid):
        self.resultid = resultid

    def go(self, g):
        for rid in as_iter(self.resultid):
            if not g.has_tag(rid, Avail):
                break
            g.remove_tag(rid, Avail)
            g.add_tag(Failed, rid)
            operatorid = g.neighbor(rid, port_label='source')
            if not operatorid:
                break
            g.add_tag(Failed, operatorid)
            operands = g.neighbors(operatorid, port_label='source')
            for operand in operands:
                g.add_tag(Avail, operand)
                g.remove_tag(operand, Consumed)

# Definition of a Numbo problem, a "numble"

class Numble:

    def __init__(self, bricks, target):
        'bricks: a list of integers. target: an integer.'
        self.bricks = bricks
        self.target = target

    def build(self, g, container):
        '''Builds the nodes for the numble as members of the container node
        in graph g. Returns container.'''
        target_id = g.make_node(Target(self.target), container)
        WantBuiltFromBricks.add_tag(g, target_id)
        g.graph['target'] = target_id
        for brick in self.bricks:
            brick_id = g.make_node(Brick(brick), container)
            #TagWith(Avail, taggee=brick_id).go(g)
            g.add_tag(Avail, brick_id)
        return container

    def as_dict(self):
        return { 'bricks': self.bricks, 'target': self.target }

    __repr__ = nice_object_repr


def prompt_for_numble():
    '''Prompts the user to enter bricks and a target at the keyboard.
    Returns a Numble object, or None if user just hit Enter.'''
    print()
    try:
        while True:
            brick_str = input('Bricks: ')
            if not brick_str:
                return None
            try:
                bricks = [int(b) for b in brick_str.split()]
                if not bricks:
                    continue #TODO Probably better to throw an exception
                break
            except ValueError:
                print('Please enter the bricks as integers separated by spaces.')
                continue

        while True:
            target_str = input('Target: ')
            if not target_str:
                return None
            try:
                target = int(target_str)
                break
            except ValueError:
                print('Please enter one integer and press Enter.')
                continue

        return Numble(bricks, target)
    except EOFError:
        print()
        return None
