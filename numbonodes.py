# numbonodes.py -- Class definitions for nodes needed by Numbo

from PortGraph import Node, Number


class Workspace(Node):
    pass

class Brick(Number):
    pass
class Target(Number):
    pass

class Operator(Node):
    def symbol(self):
        return '?'

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
