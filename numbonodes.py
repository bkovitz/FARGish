# numbonodes.py -- Class definitions for nodes needed by Numbo

from PortGraph import Node


class Workspace(Node):
    pass


class Number(Node):

    def __init__(self, n):
        self.value = n

    def is_attrs_match(self, other):
        try:
            return self.value == other.value
        except AttributeError:
            return False

    def expr_str(self, g, node):
        source = g.neighbor(node, 'source')
        if source is None:
            return str(self)
        source_datum = g.datum(source)
        return source_datum.expr_str(g, source) + ' = ' + str(self)

class Brick(Number):
    pass
class Target(Number):
    pass

class Operator(Node):
    def symbol(self):
        return '?'

    def expr_str(self, g, node):
        sources = g.neighbors(node, 'source')
        sep = ' ' + self.symbol() + ' '
        return sep.join(g.datum(source).expr_str(g, source)
                           for source in sources)

class Plus (Operator):
    def symbol(self):
        return '+'

class Minus (Operator):
    def symbol(self):
        return '-'

class Times (Operator):
    def symbol(self):
        return '*'

class Div (Operator):
    def symbol(self):
        return '/'
        
Operator.operator_classes = {'+': Plus, '-': Minus, '*': Times, '/': Div}


class Equation(Node):
    def __init__(self, name):
        self.name = name
