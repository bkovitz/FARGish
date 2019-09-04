# numbonodes.py -- Class definitions for nodes needed by Numbo

from PortGraph import Node, Tag
from watcher import Watcher, Response, TagWith #TODO shouldn't need TagWith
import expr

from abc import ABC, abstractmethod
from random import shuffle, choice
from itertools import permutations, product


class Workspace(Node):
    pass


class Avail(Tag):
    pass

class Wanted(Tag, Watcher):

    def look(self, g, this_tag):
        avails = g.nodes_with_tag(Avail)
        wanteds = g.neighbors(this_tag, port_label=self.tag_port_label)
        return [FoundWanted(avail, wanted)
                    for avail, wanted in product(avails, wanteds)
                        if (avail != wanted
                            and
                            g.have_same_value(avail, wanted)
                           )
        ]

class FoundWanted(Response):

    def __init__(self, avail, wanted):
        self.avail = avail
        self.wanted = wanted

    def go(self, g):
        g.add_edge(self.avail, 'consumer', self.wanted, 'source')
        g.remove_tag(self.avail, Avail)
        Avail.add_tag(g, self.wanted)
        g.replace_tag(self.wanted, Wanted, WasWanted)

    def annotation(self, g):
        return (
            "Hey, look! We wanted %s and we have %s."
            %
            (g.datumstr(self.wanted), g.datumstr(self.avail))
        )


class WasWanted(Tag):
    pass

class Failed(Tag):
    pass

class CloseNumbers(Tag):
    pass

class Seek(Wanted):
    pass


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

    def fail(self, g, node):
        pass

    def cascade_fail(self, g, node):
        'Called by an upstream node that has failed.'
        TagWith(Avail, taggee=node).go(g) #TODO add_tag

class Brick(Number):
    pass

class Target(Number, Watcher):
    needs_source = True

    def look(self, g, node, nodes=None):
        if g.is_fully_sourced(node):
            g.set_done(NumboSuccess(g, node))
        return []


class Block(Number):

    needs_source = True

    def expr(self, g, node):
        source = g.neighbor(node, port_label='source')
        #TODO What if there's more than one source? Or none?
        return g.expr(source)

    def fail(self, g, node):
        TagWith(Failed, taggee=node).go(g) #TODO need add_tag function
        g.remove_tag(node, Avail)
        for source in g.neighbors(node, 'source'):
            g.remove_edge(node, 'source', source, 'consumer')
            g.add_edge(node, 'former_source', source, 'former_consumer')
            g.cascade_fail(source)


class Operator(Node, ABC):

    needs_source = True

    def symbol(self):
        return '?'

    @abstractmethod
    def result_value(self, g, node):
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

    def symbol(self):
        return '+'

    def result_value(self, g, node):
        #TODO OAOO
        operands = list(g.neighbors(node, 'source'))
        if operands:
            return sum(g.value_of(operand) for operand in operands)
        else:
            return None

class Minus (Operator):

    expr_class = expr.Minus

    def symbol(self):
        return '-'

class Times (Operator):

    expr_class = expr.Times

    def symbol(self):
        return '*'

    def result_value(self, g, node):
        operands = list(g.neighbors(node, 'source'))
        if operands:
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

class Equation(Node):
    def __init__(self, name):
        self.name = name


class OperandsCouldMakeTagger(Node, Watcher):

    def look(self, g, node, nodes=None):
        #TODO Don't look at everything. Weighted choice by salience, maybe
        # other factors, too.
        avails = g.nodes_with_tag(Avail, nodes=nodes)
        possible_operands = list(
            g.nodes_without_tag(
                OperandsCouldMake, nodes=avails, taggee_port_label='could_make'
            )
        )
        if len(possible_operands) >= 2:
            possible_operand_pairs = list(permutations(possible_operands, 2))
            shuffle(possible_operand_pairs)
            for operands in possible_operand_pairs:
                datum = OperandsCouldMake.maybe_make_datum(g, operands)
                if datum:
                    return [Build(datum)]
        return []


class Build(Response):

    def __init__(self, datum):
        self.datum = datum

    def go(self, g):
        self.datum.build(g)


class OperandsCouldMake(Tag):

    @classmethod
    def maybe_make_datum(cls, g, operands):
        '''Returns an OperandsCouldMake object for operands if possible;
        otherwise None. Does not build a node.'''
        possible_operators = [
            op for op in all_operators
                if not op.failed_with_these_operands(g, operands)
        ]
        if possible_operators:
            return OperandsCouldMake(operands, choice(possible_operators))
        else:
            return None

    def __init__(self, operands, operator_class):
        self.operands = operands  # nodes, not values
        self.operator_class = operator_class
        self.operator = None

    def build(self, g):
        self.operator = self.operator_class()
        operator_id = g.make_node(self.operator)
        for operand in self.operands:
            g.add_edge(operand, 'consumer', operator_id, 'source')
            g.remove_tag(operand, Avail)
        result_value = self.operator.result_value(g, operator_id)
        result_id = g.make_node(Block(result_value))
        g.add_edge(operator_id, 'consumer', result_id, 'source')
        Avail.add_tag(g, result_id)
