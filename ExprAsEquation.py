# ExprAsEquation.py -- A mix-in for ActiveGraph, to make Expr objects from nodes

from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable

import expr
from Node import MaybeNRef
from ActiveGraph import ActiveGraph

class ExprAsEquation(ActiveGraph):
    '''Mix-in for PortGraph. Provides a method expr_as_equation.'''

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def expr_as_equation(self, target):
        '''Returns an expr.Equation representing the expression whose ultimate
        'consumer' is target.'''
        source = self.neighbor(target, port_label=['source', 'operands'])
        #print('EXPR_AS', source)
        if source:
            source_expr = self.expr(source)
        else:
            source_expr = expr.Number(self.value_of(target))
        #print('SOURCE_EXPR', source_expr)
        return expr.Equation(
            source_expr,
            expr.Number(self.value_of(target))
        )

    def expr_in_progress(self, nref: MaybeNRef) \
    -> Union[expr.Expr, expr.Equation]:
        '''Returns an expr.Expr for nref if nref is a Brick; otherwise an
        expr.Equation whose rhs is nref. This is helpful for seeing what
        expressions are currently being explored. nref should be a Number.'''
        if self.is_of_class(nref, 'Brick'):
            return expr.Number(self.value_of(nref))
        else:
            return expr.Equation(
                self.expr(nref),
                expr.Number(self.value_of(nref))
            )

    def expr(self, node) -> expr.Expr:
#        if node is None:
#            return expr.UnspecifiedExpr()
#        else:
#            #print('NODE', node, self.datum(node))
#            return self.datum(node).expr(self, node)

        #HACK Should do this by inheritance, but it's pretty readable
        #having it all here.
        if self.is_of_class(node, 'Plus'):
            return expr.Plus(
                *(self.expr(rand)
                    for rand in self.neighbors(node, 'operands'))
            )
        elif self.is_of_class(node, 'Minus'):
            return expr.Minus(
                self.neighbor(node, 'minuend'),
                self.neighbor(node, 'subtrahend')
            )
        elif self.is_of_class(node, 'Times'):
            return expr.Times(
                *(self.expr(rand)
                    for rand in self.neighbors(node, 'operands'))
            )
        #TODO Div
        elif self.is_of_class(node, 'Block'):
            return self.expr(self.neighbor(node, 'source'))
        elif self.is_of_class(node, 'Number'):
            return expr.Number(self.value_of(node))
        else:
            return expr.UnspecifiedExpr()
