# equation.py -- Watcher and Response classes to recognize and complete
#                partial instances of equations

from watcher import Watcher, Response, TagWith
from PortGraph import CouldMake, pn
from numbonodes import Equation
from submatch import matching_subgraphs

from collections.abc import Iterable


def is_operand(g, node):
    return any(hop.to_port_label == 'operands'
                   for hop in g.hops_from_port(node, 'result'))

def operands_of(g, n):
    if isinstance(n, Iterable):
        return [node for node in n if is_operand(g, node)]
    else:
        return operands_of(g, g.members_of(n))

#TODO Rename to OperandsWatcher?
class EquationWatcher(Watcher):
    
    def __init__(self, bg, eqn_node):
        self.bg = bg
        self.eqn_node = eqn_node
        self.operands = bg.subgraph(operands_of(bg, eqn_node))

    def look(self, hg):
        return [self.make_response(bindings)
                    for bindings in matching_subgraphs(self.operands, hg)
               ]

    def make_response(self, bindings):
        edges = {
            'by_completing': self.eqn_node,
            'from': bindings.values(),
            'make': self.bg.find_member_in_role(self.eqn_node, 'result')
        }
        return TagWith(CouldMake, (bindings,), edges)


class CompleteEquation(Response):

    def __init__(self, nodes):
        pass
    
#    def go(self):
#        for base_node in self.eqn:
#            if base_node not in bindings:
#                new_node_id = g.dup_node(eqn, 
        
    def make_response(self, binding):
        def complete_equation(hg):
            return "TODO"
        return complete_equation
