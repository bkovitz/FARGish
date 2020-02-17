# raw.py -- Classes holding 'raw' parsed elements of FARGish, generated
#           directly by the syntactic analyzer in grammar.py
#
# These classes, in turn, can generate Python code.

from abc import ABC, abstractmethod
from pprint import pprint as pp

from Env import EnvItem
from LinkSpec import LinkSpec
from NodeSpec import BuildSpec
from util import NiceRepr, newline
from exc import NoUniqueMateError

def as_name(x):
    try:
        return x.name
    except KeyError:
        return x

class ExternalName(EnvItem):
    def __init__(self, name):
        self.name = name

    def add_to_env(self, env):
        env.add(self.name, self)
        
class ExternalList(EnvItem):
    def __init__(self, names):
        '''names: a list of external function names'''
        self.names = [ExternalName(name) for name in names]

    def add_to_env(self, env):
        for name in self.names:
            name.add_to_env(env)

class PortLabel(EnvItem):
    def __init__(self, name):
        self.name = name
        self.links_to = set()   # a set of PortLabels

    def add_to_env(self, env):
        env.add(self.name, self)

    def add_link_to(self, other):
        '''other should be another PortLabel.'''
        self.links_to.add(other)

    def unique_mate(self, env):
        '''Returns unique PortLabel that is the mate to this PortLabel or
        raises a NoUniqueMateError.'''
        #TODO There must be a way to deduce an appropriate mate when there
        #is more than one possibility.
        if len(self.links_to) == 0:
            raise NoUniqueMateError(
                f"There is no mate defined for port label '{self.name}'."
            )
        elif len(self.links_to) == 1:
            return list(self.links_to)[0]
        else:
            mates = ', '.join(f"'{m.name}'" for m in self.links_to)
            raise NoUniqueMateError(
f"Port label '{self.name}' has multiple mates defined: {mates}."
            )

class LinkDefn(EnvItem):
    def __init__(self, from_label, to_label):
        self.from_label = PortLabel(from_label)
        self.to_label = PortLabel(to_label)

    def add_to_env(self, env):
        plfrom = env.get_or_add(self.from_label.name, self.from_label)
        plto = env.get_or_add(self.to_label.name, self.to_label)
        plfrom.add_link_to(plto)
        plto.add_link_to(plfrom)

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

class NodeDefn(EnvItem):
    def __init__(self, name, args, body, ancestors):
        self.name = name
        self.args = args
        self.body = body
        self.ancestors = ancestors  # names, not NodeDefns

    def add_to_env(self, env):
        env.add(self.name, self)

    #TODO UT
    def true_args(self, env):
        result = []
        for ancestor in self.ancestors:
            #TODO Report error of ancestor undefined
            #TODO Catch circularity
            result += env[ancestor].true_args(env)
        result += self.args
        return result

    def gen(self, file, env, fixup):
        #TODO Somewhere check that the args actually passed are appropriate 
        #to the nodeclass.

        # Code for auto-linking
        lss = self.link_specs(env)
        if lss:
            link_spec_code = '\n'.join(gen_link_spec(ls) for ls in lss)
            lss_code = f'''    link_specs = [
{link_spec_code}
    ]
'''
            auto_link_code = '\n'.join(gen_auto_link(ls) for ls in lss)
            als_code = f'''
    def auto_link(self, thisid, g):
{auto_link_code}
'''
        else:
            lss_code = ''
            als_code = ''

        # Node arguments
        targs = self.true_args(env)
        if targs:
            inargs = ', '.join(f'{t}=None' for t in targs)
            absorb = '\n'.join(f"        kwargs['{t}'] = {t}" for t in targs)
            init_code = f'''
    def __init__(self, {inargs}, **kwargs):
{absorb}
        super().__init__(**kwargs)
'''
        else:
            init_code = ''

        # Actions
        build_specs = []
        actions = []
        for body_item in self.body:
            body_item.body_gen(build_specs, actions)

        if build_specs:
            bss_code = f'''
{self.name}.build_specs = [
{newline.join("    " + b + "," for b in build_specs)}
]
'''
            a_code = f'''
    def actions(self, g, thisid):
        return [
            spec.maybe_make_build_action(g, thisid)
                for spec in self.build_specs
        ]
'''
        else:
            bss_code = ''
            a_code = ''
    
        # Now put all the generated code together to define the node class
        if self.ancestors:
            ancs = ', '.join(self.ancestors)
        else:
            if a_code:
                ancs = 'ActiveNode'
            else:
                ancs = 'Node'

        if lss_code or init_code or als_code or a_code:
            body_code = f'{lss_code}{init_code}{als_code}{a_code}'
        else:
            body_code = '    pass\n'

        print(f'''class {self.name}({ancs}):
{body_code}''', file=file)
        if bss_code:
            print(bss_code, file=fixup)

    def link_specs(self, env):
        result = []
        for arg in self.args:
            argdef = env.get(arg)
            if isinstance(argdef, PortLabel):
                result.append(
                    LinkSpec(argdef.unique_mate(env).name, argdef.name)
                )
        return result

def gen_link_spec(link_spec):
    return f'''        {repr(link_spec)},'''

def gen_auto_link(link_spec):
    return f'''        _otherid = self.{link_spec.new_node_port_label}
        if _otherid:
            g.add_edge(thisid, '{link_spec.new_node_port_label}', _otherid, '{link_spec.old_node_port_label}')'''

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
    def body_gen(self, build_specs, actions):
        build_specs.append(
            f"BuildSpec({as_name(self.expr)}, LinkSpec('agents', 'behalf_of'))"
        )

class ArgExpr(NiceRepr):
    def __init__(self, argname, expr):
        self.argname = argname
        self.expr = expr
