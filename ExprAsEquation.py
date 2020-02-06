# ExprAsEquation.py -- A mix-in for PortGraph, to make Expr objects from nodes

import expr

class ExprAsEquation:
    '''Mix-in for PortGraph. Provides a method expr_as_equation.'''

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def expr_as_equation(self, target):
        '''Returns an expr.Equation representing the expression whose ultimate
        'consumer' is target.'''
        source = self.neighbor(target, port_label=['source', 'operands'])
        print('EXPR_AS', source)
        if source:
            source_expr = self.expr(source)
        else:
            source_expr = expr.Number(self.value_of(target))
        print('SOURCE_EXPR', source_expr)
        return expr.Equation(
            source_expr,
            expr.Number(self.value_of(target))
        )

    def expr(self, node):
        if node is None:
            return expr.UnspecifiedExpr()
        else:
            #print('NODE', node, self.datum(node))
            return self.datum(node).expr(self, node)