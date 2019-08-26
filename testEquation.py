# testEquation.py -- Unit tests for Equation

# TODO Put actual unit tests in this file. Move generic code from the __main__
# test at the end to appropriate source files.

from equation import EquationWatcher

from PortGraph import PortGraph, pg
from numbonodes import *

from inspect import isclass


def make_numble(g, bricks, target):
    '''Makes a Workspace node containing numble and returns the Workspace's
    node id.'''
    ws = g.make_node(Workspace)
    for brick in bricks:
        brick_id = g.make_node(Brick(brick))
        g.add_member_edge(ws, brick_id)
    target_id = g.make_node(Target(target))
    g.add_member_edge(ws, target_id)
    return ws

def make_equation(g, operands, operator, result):
    if isclass(operator):
        operator = operator()
    operator_id = g.make_node(operator)
    result_id = g.make_node(Number(result))
    g.add_edge(operator_id, 'result', result_id, 'source')

    equation_id = g.make_node(Equation('%s=%s' % (
                                           operator.symbol().join(
                                              str(operand)
                                                  for operand in operands
                                           ),
                                           str(result))))
    g.add_member_edge(equation_id, operator_id)
    g.add_member_edge(equation_id, result_id)

    result = {'operator_id': operator_id,
              'result_id': result_id,
              'operand_ids': set(),
              'equation_id': equation_id}
    for operand in operands:
        operand_id = g.make_node(Number(operand))
        g.add_edge(operand_id, 'result', operator_id, 'operands')
        g.add_member_edge(equation_id, operand_id)
        result['operand_ids'].add(operand_id)
    return result

if __name__ == '__main__':
    g = PortGraph()
    ws = g.members_to_subgraph(make_numble(g, [1, 1], 2))
    eqn = make_equation(g, [1, 1], Plus, 2)
    pg(g)
    print()
    watcher = EquationWatcher(g, eqn['equation_id'])
    saw = watcher.look(ws)
    saw[0].go(g)
    pg(g)
