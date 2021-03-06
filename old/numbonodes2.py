# numbonodes.py -- Class definitions for nodes needed by Numbo

from PortGraph import Node, Tag, NodesWithSalience, pg, ps
from watcher import Watcher, Response, Decision, TagWith2
from View import View, NodeCriterion
from exc import NumboSuccess
import expr
from util import as_iter, sample_without_replacement
from log import ShowOperandCandidates

from abc import ABC, abstractmethod
from random import shuffle, choice, choices, randrange, sample
from itertools import permutations, combinations, product, chain
from collections import Counter
from operator import itemgetter


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

    #TODO rm
    def expr_str(self, g, node):
        source = g.neighbor(node, 'source')
        if source is None:
            return str(self)
        source_datum = g.datum(source)
        return source_datum.expr_str(g, source) + ' = ' + str(self)

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

#    def look(self, g, node, nodes=None):
#        if g.is_fully_sourced(node):
#            return [HaltNumbo(NumboSuccess(g, node))]
#        return []

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
all_operators = [Plus, Times]  #TODO Minus: need to represent the order of
                               #the operands in the graph.

class Avail(Tag):
    pass

class Failed(Tag):
    pass

def maybe_give_support(
    responses, g, from_node, to_node, weight=0.1, mutual=False
):
    '''Appends a GiveSupport object if from_node does not already support
    to_node.'''
    if not g.supports(from_node, to_node):
        if mutual:
            responses.append(
                StartMutualSupport(from_node, to_node, weight=weight)
            )
        else:
            responses.append(GiveSupport(from_node, to_node, weight=weight))

def wanted_value(g, wanted):
    '''wanted is a node id. Returns the value of the node tagged by wanted.'''
    return g.value_of(g.neighbor(wanted, port_label='taggees'))

class Wanted(Tag, Watcher):

    initial_support_for = 1.0

    def OLDlook(self, g, this_tag):
        candidates = g.nodes
        wanteds = g.neighbors(this_tag, port_label=self.tag_port_label)
        responses = []
        for candidate, wanted in product(candidates, wanteds):
#            if candidate != wanted and g.have_same_value(candidate, wanted):
#                if g.has_tag(candidate, Avail):
#                    responses.append(ConsumeSource(candidate, wanted))
#                    maybe_give_support(responses, g, this_tag, candidate)
#                elif could_be_made(g, candidate):
#                    maybe_give_support(responses, g, wanted, candidate)
            if candidate != wanted:
                if g.has_tag(candidate, Avail):
                    if g.have_same_value(candidate, wanted):
                        responses.append(ConsumeSource(candidate, wanted))
                        maybe_give_support(responses, g, this_tag, candidate)
                elif any(
                    g.datum(c).getting_closer(g, g.value_of(wanted))
                        for c in could_be_made_by(g, candidate)
                            if isinstance(g.datum(c), CouldMakeFromOperands)
                ):    #TODO Inefficient
                    maybe_give_support(responses, g, this_tag, candidate, 0.1)
                    #TODO: weight should vary according to closeness
        return responses

    def look(self, g, this_tag):
        wanteds = g.neighbors(this_tag, port_label=self.tag_port_label)
        responses = []
        for wanted in wanteds:
            oscout = OperandsScout(this_tag)  # used to be wanted
            if oscout.ok_to_build(g):
                responses.append(Build(oscout))
            for tag in g.tags_of(wanted):
                if g.is_of_class(tag, SameNumber):
                    for taggee in g.taggees_of(tag):
                        if g.has_tag(taggee, Avail):
                            responses.append(ConsumeSource(taggee, wanted))
                        if not g.has_tag(taggee, Consumed):
                            maybe_give_support(responses, g, this_tag, taggee)
                elif g.is_of_class(tag, CloseTo):
                    maybe_give_support(responses, g, this_tag, tag)
        for tag in g.tags_of(this_tag):
                if g.is_of_class(tag, GettingCloser):
                    maybe_give_support(responses, g, this_tag, tag)
                elif g.is_of_class(tag, NotGettingCloser):
                    maybe_give_support(
                        responses,
                        g,
                        this_tag,
                        g.neighbor(tag, port_label='taggees'),
                        weight=-0.2
                    )
        return responses


def getting_closer(g, candidate, wanted_value):
    raise "Don't call this"
    #print('DAT', wanted_value, candidate, g.datum(candidate))
    result_id = g.neighbor(candidate, port_label='result')
    operator_ids = g.neighbors(candidate, port_label='operators') #WRONG
    result_value = g.value_of(result_id)
    result_distance = wanted_value - result_value  # TODO abs
    if result_distance < 0:
        return False  # HACK until Minus is implemented
    for operator_id in operator_ids:
        if result_distance < (wanted_value - g.value_of(operator_id)):
            return True
    return False


class WantFullySourced(Wanted):

    min_support_for = 4.0

    def look(self, g, this_tag):
        wanteds = g.neighbors(this_tag, port_label=self.tag_port_label)
        responses = [
            HaltNumbo(NumboSuccess(g, wanted))
                for wanted in wanteds
                    if g.is_fully_sourced(wanted)
        ]
        return responses + super().look(g, this_tag)


class Consumed(Tag):
    pass


def consume(g, avail, result=None):
    '''Replaces Avail tag(s) on avail node(s) with Consumed tags.
    Adds Avail tag(s) to result node(s), removing any Consumed tags.'''
    for node in as_iter(avail):
        g.remove_tag(node, Avail)
        g.add_tag(Consumed, node)
    for node in as_iter(result):
        g.remove_tag(node, Consumed)
        if not g.is_of_class(node, Target):  #HACK
            g.add_tag(Avail, node)

    #A model HACK: "reset" the race for support to exceed the threshold
    for node in g.nodes_of_class(CouldMake):
        g.set_support_for(node, min(0.01, g.support_for(node)))


class CouldMake(Tag):
    # abstract class
    pass

def could_be_made_by(g, node):
    #TODO Exploit inheritance from CouldMake, which should have its connecting
    #port labels as attributes.
    return (t for t in g.neighbors(node, port_label='could_make')
                if (
                    g.has_edge(t, 'result', node, 'could_make')
                    and
                    not g.has_tag(t, Failed)
                )
            )

def could_be_made(g, node):
    '''Is node tagged by a non-Failed CouldMake tag?'''
    try:
        next(could_be_made_by(g, node))
        return True
    except StopIteration:
        return False


class ConsumeSource(Decision):
    '''Consumes avail, making wanted its consumer and avail wanted's source.'''

    action_threshold = 0.5

    def __init__(self, avail, wanted):
        self.avail = avail
        self.wanted = wanted

    def go(self, g):
        consume(g, self.avail, self.wanted)
        g.add_edge(self.avail, 'consumer', self.wanted, 'source')

    def annotation(self, g):
        return (
            "Hey, look! We wanted %s and we have %s."
            %
            (g.datumstr(self.wanted), g.datumstr(self.avail))
        )

class GiveSupport(Response):

    def __init__(self, from_node, to_node, weight=0.1):
        self.from_node = from_node
        self.to_node = to_node
        self.weight = weight

    def go(self, g):
        g.add_support(self.from_node, self.to_node, self.weight)

    def annotation(self, g):
        return "%s is now supporting %s" % (
            g.datumstr(self.from_node),
            g.datumstr(self.to_node)
        )

class StartMutualSupport(Response):

    def __init__(self, from_node, to_node, weight=0.1):
        self.from_node = from_node
        self.to_node = to_node
        self.weight = weight

    def go(self, g):
        g.add_mutual_support(self.from_node, self.to_node, self.weight)

    def annotation(self, g):
        return "There is now mutual support between %s and %s." % (
            g.datumstr(self.from_node),
            g.datumstr(self.to_node)
        )

class HaltNumbo(Response):

    def __init__(self, done_object):
        self.done_object = done_object

    def go(self, g):
        g.set_done(self.done_object)

    def annotation(self, g):
        return str(self.done_object)


def could_be_operand(g, node):
    return (
        g.is_of_class(node, Number)
        and
        not g.has_tag(node, Consumed)
        and
        g.has_tag(node, Avail)  # HACK
        and
        not g.is_of_class(node, Target)  # HACK
    )

class BottomUpOperandScout(Node, Watcher):
    '''Looks for Numbers that could be operands. Response: builds a
    CouldMakeFromOperands for them.'''
    
    min_support_for = 0.1

    def look(self, g, this_node):
        candidates = NodesWithSalience(
            g, [node for node in g.nodes if could_be_operand(g, node)]
        )
        if ShowOperandCandidates.is_logging():
            print(' operand candidates: %s' % (
                ', '.join(g.nodestr(o) for o in candidates.nodes)
            ))
        if len(candidates) >= 2:
            for attempt in range(3):
                #num = randrange(2, len(candidates) + 1)
                num = 2
                #operands = sample(candidates, num)
                operands = list(candidates.choose(num))
                if ShowOperandCandidates.is_logging():
                    print(' chose: %s' % (
                        ', '.join(g.nodestr(o) for o in operands)
                    ))
                datum = CouldMakeFromOperands.maybe_make_datum(g, operands)
                if datum:
                    return [Build(datum)]
        return []

class CouldMakeFromOperands(CouldMake, Watcher):

    default_salience = 0.01

    @classmethod
    def maybe_make_datum(cls, g, operands):
        '''Returns a CouldMakeFromOperands object for operands if possible;
        otherwise None. Does not build a node.'''
        possible_operators = [
            op for op in all_operators
                if not cls.already_tagged(g, operands, operator_class=op)
                #if not op.failed_with_these_operands(g, operands)
        ]
        #NEXT Not failed_with_these_operands, but did we already tag
        # these operands with that operator?
        if ShowOperandCandidates.is_logging():
            print('  possible_operators:', possible_operators)
        if possible_operators:
            return CouldMakeFromOperands(operands, choice(possible_operators))
        else:
            return None

    @classmethod
    def already_tagged(cls, g, operands, operator_class):
        tags = list(g.tags_of(operands, cls, 'could_make'))
        operands = set(operands)
        operand_values = Counter(g.value_of(o) for o in operands)
        return any(tag for tag in g.tags_of(operands, cls, 'could_make')
                           if (
                               Counter(g.value_of(n) for n in
                                 g.neighbors(tag, port_label='operands'))
                               ==
                               operand_values
                               #set(g.neighbors(tag, port_label='operands'))
                               #==
                               #operands
                              ) and g.is_of_class(
                                       g.neighbor(tag, port_label='operator'),
                                       operator_class
                              )
        )

    def __init__(self, operands, operator_class):
        self.operands = operands  # nodes, not values
        self.operator_class = operator_class
        self.operator = None  # datum, not node
        self.operator_id = None  # node, not datum
        self.result_value = None

    def build(self, g):
        if not self.already_tagged(g, self.operands, self.operator_class):
            node = g.make_node(self)
            self.operator = self.operator_class()
            self.operator_id = g.make_node(self.operator)
            self.result_value = self.operator.calculate(
                [g.value_of(o) for o in self.operands]
            )
            result_id = g.make_node(Block(self.result_value))
            for operand in self.operands:
                g.add_edge(node, 'operands', operand, 'could_make')
                g.add_support(node, operand)
            g.add_edge(node, 'operator', self.operator_id, 'could_make')
            g.add_support(node, self.operator_id)
            g.add_edge(node, 'result', result_id, 'could_make')
            g.add_mutual_support(node, result_id)
            return node

    def getting_closer(self, g, wanted_value):
        '''Is the result closer to the wanted_value than all operands?'''
        result_distance = wanted_value - self.result_value  # TODO abs
        if result_distance < 0:
            return False  # HACK until Minus is implemented
        return all(
            result_distance < (wanted_value - g.value_of(oid))
                for oid in self.operands
        )
#        for operand_id in self.operands:
#            if result_distance < (wanted_value - g.value_of(operand_id)):
#                return True
#        return False


    #TODO node -> this_node
    def look(self, g, node, nodes=None):
        if g.has_tag(self.operator_id, Failed):
            return []
        all_operands_avail = all(g.has_tag(o, Avail) for o in self.operands)
        if not g.all_have_tag(Avail, self.operands):
            return []
        responses = [ConsumeOperands(node)]
        for wanted in g.nodes_of_class(Wanted):
            v = wanted_value(g, wanted)
            if self.getting_closer(g, v):
                if not g.has_tag(node, GettingCloser):
                    responses.append(Build(GettingCloser(node, wanted)))
            else:
                if not g.has_tag(node, NotGettingCloser):
                    responses.append(Build(NotGettingCloser(node, wanted)))
        return responses

    def datumstr(self, g, this_node):
        result = self.result_node(g, this_node)
        if result is None:
            result = self.result_value
        else:
            result = g.nodestr(result)
        rator = self.operator_id
        if rator is None:
            rator = self.operator_class.__name__
        else:
            rator = g.nodestr(rator)
        return '%s(operands=%s, operator=%s, result=%s)' % (
            self.__class__.__name__,
            ', '.join(g.nodestr(o) for o in self.operands),
            rator,
            result
        )

    def result_node(self, g, this_node):
        return g.neighbor(this_node, 'result')

    def annotation(self, g, this_node):
        return (
            'Could ' + self.equation_str(g, this_node) + ' be relevant?'
        )

    def equation_str(self, g, this_node):
        result = g.datum(g.neighbor(this_node, 'result'))
        operand_values = [g.datum(o).value for o in self.operands]
        sep = ' ' + self.operator.symbol() + ' '
        return '%s = %s' % (
            sep.join(str(v) for v in operand_values),
            result.value
        )

class MakeCouldMakeFromOperands(Decision):

    def __init__(self, g, operator_id, lbrick, rbrick):
        self.operator_id = operator_id
        self.lbrick = lbrick
        self.rbrick = rbrick
        so = g.support_for(operator_id)
        sl = g.support_for(g.tag_of(lbrick, tagclass=UseAsLeftOperand))
        sr = g.support_for(g.tag_of(rbrick, tagclass=UseAsRightOperand))
        #print('SAL', so, sl, sr)
        if min(so, sl, sr) < self.action_threshold / 2.0:
            self.salience = min(so, sl, sr)
        else:
            self.salience = so + sl + sr
#        self.salience = so + sl + sr

    def go(self, g):
        CouldMakeFromOperands(
            [self.lbrick, self.rbrick],
            g.class_of(self.operator_id)
        ).build(g)


class ConsumeOperands(Decision):

    action_threshold = 0.5

    def __init__(self, could_make):
        'could_make: the CouldMakeFromOperands node id.'
        self.could_make = could_make
    
    def go(self, g):
        operands = g.neighbors(self.could_make, 'operands')
        result = g.neighbor(self.could_make, 'result')
        operator = g.neighbor(self.could_make, 'operator')
        all_operands_avail = all(g.has_tag(o, Avail) for o in operands)
        #TODO What if some but not all operands are Avail?
        consume(g, operands)
        for operand in operands:
            g.add_edge(operand, 'consumer', operator, 'source')
        g.add_edge(operator, 'consumer', result, 'source')
        if all_operands_avail:
            Avail.add_tag(g, result)

    def gstr(self, g):
        return '%s(could_make=%s, salience=%.3f)' % (
            self.__class__.__name__,
            g.nodestr(self.could_make),
            self.salience
        )

    def annotation(self, g):
        return 'Let\'s try %s' % (
            g.datum(self.could_make).equation_str(g, self.could_make)
        )

class Build(Response):

    def __init__(self, datum):
        self.datum = datum
        self.built = None

    def go(self, g):
        self.built = self.datum.build(g)

    def gstr(self, g):
        return 'Build(%s, salience=%.3f' % (
            self.datum.datumstr(g, None),
            self.salience
        )

    def annotation(self, g):
        if self.built:
            a = g.datum(self.built).annotation
            if callable(a):
                return a(g, self.built)
            else:
                return 'Built %s' % (g.nodestr(self.built))
        else:
            return 'Failed to build %s' % (self.datum,)

class GettingCloser(Tag):

    def __init__(self, taggee, closer_to):
        '''closer_to is a node_id, not a value.'''
        self.taggee = taggee
        self.closer_to = closer_to

    def build(self, g):
        this_tag = g.make_node(self)
        g.add_tag(this_tag, self.taggee)
        g.add_edge(this_tag, 'getting_closer_to', self.closer_to, 'tags')
        return this_tag

class NotGettingCloser(Tag):

    def __init__(self, taggee, not_closer_to):
        self.taggee = taggee
        self.not_closer_to = not_closer_to

    def build(self, g):
        this_tag = g.make_node(self)
        g.add_tag(this_tag, self.taggee)
        g.add_edge(this_tag, 'not_getting_closer_to',
                   self.not_closer_to, 'tags')
        return this_tag


class NumericalRelationTag(Tag):

    initial_support_for = 0.1

    @classmethod
    def test(self, g, nodes):
        '''Should return None if nodes don't have the relation, or a Node
        datum suitable for Building if they do.'''
        pass

    #TODO Allow more than two relata
    def __init__(self, lhs, rhs):
        self.lhs = lhs
        self.rhs = rhs

    def ok_to_build(self, g):
        return not g.all_share_tag(
            tagclass=self.__class__,
            nodes=[self.lhs, self.rhs]
        )

    def build(self, g):
        node = g.make_node(self)
        g.add_edge(node, 'lhs', self.lhs, 'tags')
        g.add_edge(node, 'rhs', self.rhs, 'tags')
        return node


class SameNumber(NumericalRelationTag):

    initial_support_for = 1.0

    @classmethod
    def test(cls, g, nodes):
        vs = set(g.value_of(n) for n in nodes)
        if len(vs) == 1:
            return SameNumber(nodes[0], nodes[1])
        else:
            return None

#TODO Obsolete? NumericalRelationScout
class SameNumberScout(Node, Watcher):
    '''Looks for Numbers that are the same and tags them SameNumber.'''

    min_support_for = 1.0

    def look(self, g, this_node):
        candidate_pairs = [
            p for p in combinations(g.candidate_nodes(nodeclass=Number), 2)
                if g.value_of(p[0]) == g.value_of(p[1])
                   and
                   not g.all_share_tag(SameNumber, p)
        ]
        if candidate_pairs:
            pair = choices(
                candidate_pairs,
                weights=[
                    g.salience(p[0]) + g.salience(p[1])
                        for p in candidate_pairs
                ]
            )[0]
            return [TagWith2(SameNumber, list(pair))]
        else:
            return []

#TODO OAOO
def is_close(*vs):
    hi = max(vs)
    lo = min(vs)
    diff = abs(hi - lo)
    return (diff < 5) or (diff < abs(0.05 * hi))


class CloseTo(NumericalRelationTag):

    @classmethod
    def test(cls, g, nodes):
        vs = list(g.value_of(n) for n in nodes)
        hi = max(vs)
        lo = min(vs)
        diff = abs(hi - lo)
        if (diff < 5) or (diff < abs(0.05 * hi)):
            #TODO Allow more than 2 nodes
            assert(len(nodes) == 2)
            return CloseTo(nodes[0], nodes[1])
        else:
            return None

class GreaterThan(NumericalRelationTag):

    @classmethod
    def test(cls, g, nodes):
        tuples = sorted(
            [(node, g.value_of(node)) for node in nodes],
            key=itemgetter(1)
        )
        if tuples[0][1] == tuples[1][1]:
            return None
        else:
            return GreaterThan(tuples[1][0], tuples[0][0])


# TODO: rm? Made obsolete by NumericalRelationScout?
class CloseNumbersScout(Node, Watcher):
    '''Looks for Numbers whose values are close together.'''

    min_support_for = 1.0

    def look(self, g, this_node):
        #TODO Should exclude pairs of Numbers already tagged CloseTo
        candidates = g.candidate_nodes_wsal(nodeclass=Number)
        responses = []
        for i in range(10):
            nodes = list(candidates.choose(k=2))
            if (
                self.are_close(g, nodes)
                and 
                not g.all_share_tag(tagclass=CloseTo, nodes=nodes)
            ):
                responses.append(TagWith2(CloseTo, nodes))
        return responses

    #TODO OAOO
    def are_close(self, g, nodes):
        vs = list(g.value_of(n) for n in nodes)
        hi = max(vs)
        lo = min(vs)
        diff = abs(hi - lo)
        return (diff < 5) or (diff < abs(0.05 * hi))


class NumericalRelationScout(Node, Watcher):

    min_support_for = 1.0

    possible_tagclasses = [CloseTo, GreaterThan, SameNumber]

    def look(self, g, this_node):
        #TODO Should exclude pairs of Numbers already tagged?
        candidates = g.candidate_nodes_wsal(nodeclass=Number)
        responses = []
        for attempt in range(20):
            nodes = list(candidates.choose(k=2))
            #test = choice(self.tests)
            tagclass = choice(self.possible_tagclasses)
            #datum = test.__func__(self.__class__, g, nodes)
            datum = tagclass.test(g, nodes)
            if datum and datum.ok_to_build(g):
                responses.append(Build(datum))
                if len(responses) >= 2:
                    break
        return responses


class OperandScout(Node, Watcher):  #TODO rm? replaced by OperandsScout?

    def __init__(self, operator_class):
        self.operator_class = operator_class

    def look(self, g, this_node):
        candidates = g.candidate_nodes_wsal(nodes=g.scope_of(this_node))
        responses = []
        if len(candidates) >= 2:
            pass
            #NEXT Choose nodes, build CouldMakeFromOperands.
            #But first, think about how to make this process finer-grained.
            #Pressures must flow, pressures must add.
            #Pressure for addition, pressures for operands, pressures
            #for addition of certain operands, toward certain results.

class OperandView(View):

    min_support_for = 0.1

    def __init__(self, target_id):
        super().__init__(NodeCriterion(nodeclass=Number, tagclass=Avail))
        self.target_id = target_id  # node id of Number we're trying to build

    def update_support(self, g, this_node):
        target = g.value_of(self.target_id)
        if target is None:
            return
        viewees = list(self.viewing(g, this_node))

        could_makes = set()
        for viewee in viewees:
            could_makes.update(n for n in g.neighbors(viewee)
                                  if g.is_of_class(n, CouldMakeFromOperands))
            
        ub_by_addition = sum(g.value_of(n) for n in viewees)
        if ub_by_addition < target:
            self.favor_multiplication(g, this_node, could_makes)
        elif ub_by_addition > target * 2:
            self.favor_addition(g, this_node, could_makes)

    def favor_multiplication(self, g, this_node, could_makes):
        for could_make in could_makes:
            if issubclass(self.operator_class_of(g, could_make), Times):
                g.add_support(this_node, could_make)
            else:
                g.oppose(this_node, could_make)

    def favor_addition(self, g, this_node, could_makes):
        for could_make in could_makes:
            if issubclass(self.operator_class_of(g, could_make), Plus):
                g.add_support(this_node, could_make)
            else:
                g.oppose(this_node, could_make)
        
    #TODO This code should be closer to CouldMakeFromOperands. Better yet,
    # there should be some easy way to get an attribute from a node's
    # datum by name.
    def operator_class_of(self, g, node):
        d = g.datum(node)
        if d:
            return d.operator_class

########################################################################
#
# Nodes to build up CouldMakeFromOperands guesses in stages: guesses at
# what is worth constructing a guess out of.

class UseAsOperand(Tag):
    pass

class UseAsLeftOperand(UseAsOperand):
    pass
class UseAsRightOperand(UseAsOperand):
    pass

class BigPlusSmallMakesBig(Node, Watcher):

    def look(self, g, this_node):
        # STUB
        return []

class BigPlusSmallMakesBig(Node, Watcher):

    def __init__(self, agent_for, plus):
        self.agent_for = agent_for

    def build(self, g):
        node = g.make_node(self)
        g.add_edge(self.agent_for, 'agents', node, 'agent_for')
        return node

class BrickLessThanTarget(Tag, Watcher):

    def __init__(self, agent_for, brick, target):
        self.agent_for = agent_for
        self.brick = brick
        self.target = target

    def build(self, g):
        node = g.make_node(self)
        g.add_edge(self.agent_for, 'agents', node, 'agent_for')
        g.add_edge(node, 'taggees', self.brick, 'tags')
        g.add_edge(node, 'taggees', self.target, 'tags')
        g.add_support(node, g.tag_of(self.brick, tagclass=UseAsLeftOperand))
        g.add_mutual_support(node, g.tag_of(self.brick, tagclass=UseAsRightOperand))
        return node

    def look(self, g, this_node):
        responses = []
        for agent in g.neighbors(self.agent_for, 'agents'):
            if agent == this_node:
                continue
            if g.is_of_class(agent, (Plus, Times, BrickCloseToTarget)):
                maybe_give_support(responses, g, this_node, agent, weight=0.05)
        return responses

class BrickCloseToTarget(Tag, Watcher):

    def __init__(self, agent_for, brick, target):
        self.agent_for = agent_for
        self.brick = brick
        self.target = target

    def build(self, g):
        node = g.make_node(self)
        g.add_edge(self.agent_for, 'agents', node, 'agent_for')
        g.add_edge(node, 'taggees', self.brick, 'tags')
        g.add_edge(node, 'taggees', self.target, 'tags')
        g.add_mutual_support(node, g.tag_of(self.brick, tagclass=UseAsLeftOperand))
        return node

    def look(self, g, this_node):
        responses = []
        for agent in g.neighbors(self.agent_for, 'agents'):
            if agent == this_node:
                continue
            if g.is_of_class(agent, Plus):
                maybe_give_support(responses, g, this_node, agent, mutual=True)
            elif g.is_of_class(agent, TinyBrick):
                maybe_give_support(responses, g, this_node, agent, weight=0.5, mutual=True)
        return responses

class GoodLeftMultiplier(Tag, Watcher):
    
    def __init__(self, agent_for, brick, target):
        self.agent_for = agent_for
        self.brick = brick
        self.target = target

    def build(self, g):
        node = g.make_node(self)
        g.add_edge(self.agent_for, 'agents', node, 'agent_for')
        g.add_edge(node, 'taggees', self.brick, 'tags')
        g.add_edge(node, 'taggees', self.target, 'tags')
        g.add_mutual_support(node, g.tag_of(self.brick, tagclass=UseAsLeftOperand), weight=0.2)
        return node

    def look(self, g, this_node):
        responses = []
        for agent in g.neighbors(self.agent_for, 'agents'):
            if agent == this_node:
                continue
            if g.is_of_class(agent, (Times, GoodRightMultiplier)):
                maybe_give_support(responses, g, this_node, agent)
        return responses

class GoodRightMultiplier(Tag, Watcher):
    
    def __init__(self, agent_for, brick, target):
        self.agent_for = agent_for
        self.brick = brick
        self.target = target

    def build(self, g):
        node = g.make_node(self)
        g.add_edge(self.agent_for, 'agents', node, 'agent_for')
        g.add_edge(node, 'taggees', self.brick, 'tags')
        g.add_edge(node, 'taggees', self.target, 'tags')
        g.add_mutual_support(node, g.tag_of(self.brick, tagclass=UseAsRightOperand), weight=0.2)
        return node

    def look(self, g, this_node):
        responses = []
        for agent in g.neighbors(self.agent_for, 'agents'):
            if agent == this_node:
                continue
            if g.is_of_class(agent, (Times, GoodLeftMultiplier)):
                maybe_give_support(responses, g, this_node, agent)
        return responses

class TinyBrick(Tag, Watcher):

    def __init__(self, agent_for, brick, target):
        self.agent_for = agent_for
        self.brick = brick
        self.target = target

    def build(self, g):
        node = g.make_node(self)
        g.add_edge(self.agent_for, 'agents', node, 'agent_for')
        g.add_edge(node, 'taggees', self.brick, 'tags')
        g.add_edge(node, 'taggees', self.target, 'tags')
        g.add_mutual_support(node, g.tag_of(self.brick, tagclass=UseAsRightOperand))
        return node

    def look(self, g, this_node):
        responses = []
        for agent in g.neighbors(self.agent_for, 'agents'):
            if agent == this_node:
                continue
            if g.is_of_class(agent, Plus):
                maybe_give_support(responses, g, this_node, agent, mutual=True)
            elif g.is_of_class(agent, BrickCloseToTarget):
                # HACK weight= forcing this to be high priority
                maybe_give_support(responses, g, this_node, agent, weight=0.5, mutual=True)
        return responses


class OperandsScout(Node, Watcher):

    tagclasses_to_support = (
        BrickLessThanTarget, BrickCloseToTarget, GoodLeftMultiplier,
        GoodRightMultiplier, TinyBrick
    )

    def __init__(self, agent_for):
        self.agent_for = agent_for  # the node we're working for, assumed to
                                    # be a Wanted
        self.plus = None
        self.times = None

    def ok_to_build(self, g):
        agents = g.nodes_of_class(
            self.__class__,
            g.neighbors(self.agent_for, port_label='agents')
        )
        return len(agents) == 0

    def build(self, g):
        container = g.container_of(self.agent_for)
        node = g.make_node(self, container=container)
        g.add_edge(self.agent_for, 'agents', node, 'agent_for')
        self.plus = g.make_node(Plus, container=container)
        self.times = g.make_node(Times, container=container)
        g.add_mutual_opposition(self.plus, self.times)
        #TODO 'tags' doesn't seem like the right name for this.
        g.add_edge(node, 'agents', self.plus, 'agent_for')
        g.add_edge(node, 'agents', self.times, 'agent_for')
        g.add_support(self.agent_for, node)
        #self.notice_everything(g, node) #TODO notice a little at a time
        return node

    def look(self, g, this_node):
        responses = []
        for operator_id in [self.plus, self.times]:
            for ul in g.tags_of(this_node, tagclass=UseAsLeftOperand,
                                taggee_port_label='agents'):
                #print('LOOK')
                for ur in g.tags_of(this_node, tagclass=UseAsRightOperand,
                                    taggee_port_label='agents'):
                    lbrick = g.taggee_of(ul)
                    rbrick = g.taggee_of(ur)
                    if lbrick == rbrick:
                        continue
                    r = MakeCouldMakeFromOperands(g, operator_id, lbrick, rbrick)
                    #print('RRRR', r)
                    responses.append(r)
        return responses

    def notice_everything(self, g, this_node):   # HACK !!!!!!
        #wanted_value = g.value_of(self.agent_for)
        wv = wanted_value(g, self.agent_for)
        new_nodes = []
        uls = []   # UseAsLeftOperand nodes
        urs = []   # UseAsRightOperand nodes
        operand_ids = list(g.nodes_with_tag(Avail))
        for operand_id in operand_ids:
            ul = g.add_tag(UseAsLeftOperand, operand_id)
            ur = g.add_tag(UseAsRightOperand, operand_id)
            g.add_edge(this_node, 'agents', ul, 'agent_for')
            g.add_edge(this_node, 'agents', ur, 'agent_for')
            uls.append(ul)
            urs.append(ur)
            g.add_mutual_opposition(ul, ur)
            new_nodes += [ul, ur]
        for ul1, ul2 in combinations(uls, 2):
            g.add_mutual_opposition(ul1, ul2)
        for ur1, ur2 in combinations(urs, 2):
            g.add_mutual_opposition(ur1, ur2)
        for ul, ur in product(uls, urs):
            if g.taggee_of(ul) != g.taggee_of(ur):
                g.add_mutual_support(ul, ur, weight=0.05)

        for operand_id in operand_ids:
            #NEXT Look at tags, add BrickCloseToTarget, etc.
            pass

        for ul in uls:
            brick = g.taggee_of(ul)
            v = g.value_of(brick)
            if v < wv:
                new_nodes.append(
                    BrickLessThanTarget(
                        this_node, brick, self.agent_for
                    ).build(g)
                )
            if is_close(v, wv):
                new_nodes.append(
                    BrickCloseToTarget(
                        this_node, brick, self.agent_for
                    ).build(g)
                )
            if v >= 2 and v < wv / 2:
#                new_nodes.append(
#                    GoodLeftMultiplier(
#                        this_node, brick, self.agent_for
#                    ).build(g)
#                )
                pass

        for ur in urs:
            brick = g.taggee_of(ur)
            v = g.value_of(brick)
            if abs(v) < 5 or abs(v) < abs(0.05 * wv):
                new_nodes.append(
                    TinyBrick(
                        this_node, brick, self.agent_for
                    ).build(g)
                )
            if v >= 2 and v < wv / 2:
#                new_nodes.append(
#                    GoodRightMultiplier(
#                        this_node, brick, self.agent_for
#                    ).build(g)
#                )
                pass

        for node in new_nodes:
            print('NODE', node, g.class_of(node))
            if g.is_of_class(node, self.tagclasses_to_support):
                g.add_support(this_node, node)
