# expr.py -- Classes to hold an expression as a tree
#
# Some of these classes have the same names as Numbo node classes. So,
# you should refer to them by expr.Plus, expr.Times, and not import
# them into the importing module's namespace as Plus, Times, etc.

from abc import ABC, abstractmethod, ABCMeta


class Expr(ABC):
    # Concrete classes need a numeric 'precedence' member

    is_noncommutative_operator = False

    @abstractmethod
    def raw_s(self):
        '''Should return a string representing the Expr, without regard for
        enclosing parentheses. The raw_s function should call x.s(self)
        on each subexpression. Do not call raw_s directly.'''
        pass

    def need_parens(self, parent):
        return self.precedence < parent.precedence

    def s(self, parent=None):
        '''Don't override this.'''
        if parent is None or not self.need_parens(parent):
            return self.raw_s()
        else:
            return '(' + self.raw_s() + ')'

    def __str__(self):
        return self.s()

class LeafExpr(Expr, metaclass=ABCMeta):
    '''An abstract class for Exprs that can't require parentheses.'''
    precedence = 10

    def needs_parens(self):
        return False

class UnspecifiedExpr(LeafExpr):
    '''Like a constant, but means that its value or expression is unknown.'''

    def raw_s(self):
        return '?'

class Equation(Expr):
    precedence = 0

    def __init__(self, lhs, rhs):
        self.lhs = lhs
        self.rhs = rhs

    def raw_s(self):
        return self.lhs.s(self) + ' = ' + self.rhs.s(self)

class Operator(Expr, ABC):
    #@abstract ... constants?
    # symbol = '?'
    # commutative = boolean
    def __init__(self, *operands):
        self.operands = operands

class CommutativeOperator(Operator, ABC):

    def need_parens(self, parent):
        if parent.is_noncommutative_operator:
            return self.precedence <= parent.precedence
        else:
            return self.precedence < parent.precedence

    def raw_s(self):
        if len(self.operands) == 1:
            return '(%s %s)' % (self.symbol, self.operands[0].s(self))
        else:
            sep = ' ' + self.symbol + ' '
            return sep.join(o.s(self) for o in self.operands)

class NoncommutativeOperator(CommutativeOperator, ABC):
    is_noncommutative_operator = True


class Plus(CommutativeOperator):
    symbol = '+'
    precedence = 1

class Minus(NoncommutativeOperator):
    symbol = '-'
    precedence = 1

class Times(CommutativeOperator):
    symbol = '*'
    precedence = 2

class Div(NoncommutativeOperator):
    symbol = '/'
    precedence = 2

class Number(LeafExpr):
    def __init__(self, n):
        self.n = n

    def raw_s(self):
        return str(self.n)
