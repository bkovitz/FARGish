from PortGraph import PortGraph, Node, Number
from submatch import matching_subgraphs

from collections.abc import Iterable
from random import choice


class Workspace(Node):
    pass

class Brick(Number):
    pass
class Target(Number):
    pass

class Operator(Node):
    @classmethod
    def d(cls, value):
        return {'_class': cls.operator_classes[value]}

class Plus (Operator):
    pass
class Minus (Operator):
    pass
class Times (Operator):
    pass
class Div (Operator):
    pass
        
Operator.operator_classes = {'+': Plus, '-': Minus, '*': Times, '/': Div}


class Equation(Node):
    pass


def make_numble(bricks, target):
    g = PortGraph()
    ws = g.make_node(Workspace.d())
    for brick in bricks:
        brick_id = g.make_node({'_class': Brick, 'value': brick})
        g.add_member_edge(ws, brick_id)
    target_id = g.make_node({'_class': Target, 'value': target})
    g.add_member_edge(ws, target_id)
    return g

def make_equation(g, operands, operator, result):
    operator_id = g.make_node(Operator.d(operator))
    result_id = g.make_node(Number.d(result))
    g.add_edge(operator_id, 'result', result_id, 'source')

    equation_id = g.make_node(Equation.d())
    g.add_member_edge(equation_id, operator_id)
    g.add_member_edge(equation_id, result_id)

    result = {'operator_id': operator_id,
              'result_id': result_id,
              'operand_ids': set(),
              'equation_id': equation_id}
    for operand in operands:
        operand_id = g.make_node(Number.d(operand))
        g.add_edge(operand_id, 'result', operator_id, 'operands')
        g.add_member_edge(equation_id, operand_id)
        result['operand_ids'].add(operand_id)
    return result

def is_operand(g, node):
    return any(hop.to_port_label == 'operands'
                   for hop in g.hops_from_port(node, 'result'))

def operands_of(g, n):
    if isinstance(n, Iterable):
        return [node for node in n if is_operand(g, node)]
    else:
        return operands_of(g, g.members_of(n))

def complete_equation(g, eqn, d, container):
    '''d is a subgraph binding, returned from matching_subgraphs.
    container is where to build the missing nodes.'''
    if not isinstance(eqn, PortGraph):
        eqn = g.members_to_subgraph(eqn)
    dsupp = {}
    for target_node in eqn:
        if target_node not in d:
            new_node_id = g.dup_node(eqn, target_node)
            g.add_member_edge(container, new_node_id)
            dsupp[target_node] = new_node_id
    #TODO make the edges
        

def find_equation_match(g, eqn_node, hg):
    operands = g.subgraph(operands_of(g, eqn_node))
    ms = list(matching_subgraphs(operands, hg))
    try:
        return choice(list(ms))
    except IndexError:
        return None

def pn(g):
    '''HACK: print the nodes in the graph'''
    for node in g.nodes:
        print(g.nodestr(node))

if __name__ == '__main__':
    g = make_numble([1, 1], 2)
    eqn = make_equation(g, [1, 1], '+', 2)
    ws = g.subgraph(g.members_of(1))
    pn(g)
    m = find_equation_match(g, 7, ws)
    print(m)

