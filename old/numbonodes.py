# numbonodes.py -- Class definitions for nodes needed by Numbo

from PortGraph import Node, Tag
from watcher import TagWith #TODO shouldn't need this
import expr

from abc import ABC, abstractmethod


class Workspace(Node):
    pass


class Avail(Tag):
    pass

class Wanted(Tag):
    pass

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

class Target(Number):
    needs_source = True

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
