# raw.py -- Classes holding 'raw' parsed elements of FARGish, generated
#           directly by the syntactic analyzer in grammar.py
#
# These classes, in turn, can generate Python code.

from util import NiceRepr

class ExternalList(NiceRepr):
    def __init__(self, names):
        '''names: a list of external function names'''
        self.names = names

class LinkDefn(NiceRepr):
    def __init__(self, from_label, to_label):
        self.from_label = from_label
        self.to_label = to_label

class NodeHeader(NiceRepr):
    def __init__(self, names, ancestors):
        self.names = names
        self.ancestors = ancestors
    def make_node_defns(self, body):
        return [NodeDefn(name_of(name), args_of(name), body, self.ancestors)
                    for name in self.names]

def name_of(x):
    try:
        return x.name
    except AttributeError:
        return x

def args_of(name):
    try:
        return name.args
    except AttributeError:
        return []

class NameWithArguments(NiceRepr):
    def __init__(self, name, args):
        self.name = name
        self.args = args

class NodeDefn(NiceRepr):
    def __init__(self, name, args, body, ancestors):
        self.name = name
        self.args = args
        self.body = body
        self.ancestors = ancestors

    #GEN class def
    #def gen(self, 

class Initializer(NiceRepr):
    def __init__(self, name, expr):
        self.name = name
        self.expr = expr

class BuildExpr(NiceRepr):
    def __init__(self, expr):
        self.expr = expr

class VarRef(NiceRepr):
    def __init__(self, name):
        self.name = name

class FuncCall(NiceRepr):
    def __init__(self, funcname, args):
        self.funcname = funcname
        self.args = args

class Relop(NiceRepr):
    def __init__(self, lhs, op, rhs):
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

class LetExpr(NiceRepr):
    def __init__(self, name, expr):
        self.name = name
        self.expr = expr

class SeeDo(NiceRepr):
    def __init__(self, conditions, actions, else_conditions, else_actions):
        self.conditions = conditions
        self.actions = actions
        self.else_conditions = else_conditions
        self.else_actions = else_actions

class AgentExpr(NiceRepr):
    def __init__(self, expr):
        self.expr = expr

class ArgExpr(NiceRepr):
    def __init__(self, argname, expr):
        self.argname = argname
        self.expr = expr
