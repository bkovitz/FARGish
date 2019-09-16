# numbonodes.py -- Class definitions for nodes needed by Numbo

from PortGraph import Node, Tag
from watcher import Watcher, Response, Decision
from exc import NumboSuccess
import expr
from util import as_iter
from log import ShowOperandCandidates

from abc import ABC, abstractmethod
from random import shuffle, choice, randrange, sample
from itertools import permutations, product


class Workspace(Node):
    min_support_for = 0.01  # Prevents node from being removed

class Number(Node):

    def __init__(self, n):
        self.value = n

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

def maybe_give_support(responses, g, from_node, to_node, weight=1.0):
    '''Appends a GiveSupport object if from_node does not already support
    to_node.'''
    if not g.supports(from_node, to_node):
        responses.append(GiveSupport(from_node, to_node, weight=weight))

class Wanted(Tag, Watcher):

    initial_support_for = 1.0

    def look(self, g, this_tag):
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
        g.add_tag(Avail, node)


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
    '''Is node tagged by an non-Failed CouldMake tag?'''
    try:
        next(could_be_made_by(g, node))
        return True
    except StopIteration:
        return False


class ConsumeSource(Decision):
    '''Consumes avail, making wanted its consumer and avail wanted's source.'''

    action_threshold = 1.0

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

    def __init__(self, from_node, to_node, weight=1.0):
        self.from_node = from_node
        self.to_node = to_node
        self.weight = weight

    def go(self, g):
        g.add_mutual_support(self.from_node, self.to_node, self.weight)

    def annotation(self, g):
        return "%s is now supporting %s and vice versa" % (
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
    
    min_support_for = 1.0

    def look(self, g, this_node):
        candidates = [node for node in g.nodes if could_be_operand(g, node)]
        if ShowOperandCandidates.is_logging():
            print(' operand candidates: %s' % (
                ', '.join(g.nodestr(o) for o in candidates)
            ))
        if len(candidates) >= 2:
            for attempt in range(3):
                #num = randrange(2, len(candidates) + 1)
                num = 2
                operands = sample(candidates, num)
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
    #BUG Why is this being ignored?

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
        return any(tag for tag in g.tags_of(operands, cls, 'could_make')
                           if (
                               set(g.neighbors(tag, port_label='operands'))
                               ==
                               operands
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
            g.set_support_for(node, 0.5)
            self.operator = self.operator_class()
            self.operator_id = g.make_node(self.operator)
            self.result_value = self.operator.calculate(
                [g.value_of(o) for o in self.operands]
            )
            result_id = g.make_node(Block(self.result_value))
            for operand in self.operands:
                g.add_edge(node, 'operands', operand, 'could_make')
                g.add_mutual_support(node, operand)
            g.add_edge(node, 'operator', self.operator_id, 'could_make')
            g.add_mutual_support(node, self.operator_id)
            g.add_edge(node, 'result', result_id, 'could_make')
            g.add_mutual_support(node, result_id)
            return node

    def getting_closer(self, g, wanted_value):
        '''Is the result closer to the wanted_value than at least one
        operand?'''
        result_distance = wanted_value - self.result_value  # TODO abs
        if result_distance < 0:
            return False  # HACK until Minus is implemented
        for operand_id in self.operands:
            if result_distance < (wanted_value - g.value_of(operand_id)):
                return True
        return False


    def look(self, g, node, nodes=None):
        if g.has_tag(self.operator_id, Failed):
            return []
        all_operands_avail = all(g.has_tag(o, Avail) for o in self.operands)
        if g.all_have_tag(Avail, self.operands):
            return [ConsumeOperands(node)]
        else:
            return []

    def annotation(self, g, this_node):
        return 'Got an inkling that ' + self.equation_str(g, this_node)

    def equation_str(self, g, this_node):
        result = g.datum(g.neighbor(this_node, 'result'))
        operand_values = [g.datum(o).value for o in self.operands]
        sep = ' ' + self.operator.symbol() + ' '
        return '%s = %s' % (
            sep.join(str(v) for v in operand_values),
            result.value
        )

class ConsumeOperands(Decision):

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
        self.taggee = taggee
        self.closer_to = closer_to

