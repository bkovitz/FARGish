# ExprAsEquation.py -- A mix-in for PortGraph, to make Expr objects from nodes

import expr

class ExprAsEquation:
    '''Mix-in for PortGraph. Provides a method expr_as_equation.'''

    def expr_as_equation(self, target):
        '''Returns an expr.Equation representing the expression whose ultimate
        'consumer' is target.'''
        source = self.neighbor(target, port_label='source')
        #TODO What if there's more than one source? Or none?
        return expr.Equation(self.expr(source), self.expr(target))

    def expr(self, node):
        if node is None:
            return expr.UnspecifiedExpr()
        else:
            #print('NODE', node, self.datum(node))
            return self.datum(node).expr(self, node)
