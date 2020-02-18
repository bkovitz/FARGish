# gen.py -- Classes holding 'raw' parsed elements of FARGish, generated
#           directly by the syntactic analyzer in grammar.py
#
# These classes, in turn, can generate Python code.

from abc import ABC, abstractmethod
from pprint import pprint as pp
from io import StringIO

from Env import EnvItem
from LinkSpec import LinkSpec
from NodeSpec import BuildSpec
from util import as_iter, NiceRepr, newline
from exc import NoUniqueMateError

def as_name(x):
    try:
        return x.name
    except AttributeError:
        return x

def as_expr(x):
    if hasattr(x, 'as_expr'):
        return x.as_expr()
    else:
        return str(x)

class Indent:

    prefix = '    '

    def __init__(self, *args):
        '''Indent(n, string) or Indent(string). n defaults to 1.
        str() of the Indent prepends 4 times as many spaces as n.'''
        if len(args) == 1:
            self.s = args[0]
        elif len(args) == 2:
            self.prefix = self.prefix * args[0]
            self.s = args[1]
        else:
            raise ValueError(f'Indent.__init__ accepts either one or two arguments; was passed {args}.')

    def __str__(self):
        sio = StringIO()
        for line in self.s.splitlines():
            print(f'{self.prefix}{line}', file=sio)
        return sio.getvalue()

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

class ClassVar(NiceRepr):

    def __init__(self, cl, name, expr):
        self.cl = cl  # a Class object
        self.name = name
        self.expr = expr

    def gen(self, file, fixup):
        '''The generated code installs all class variables into classes only
        after all classes are defined. This enables circular references
        between classes (since the expression for the class variable may
        invoke another class, possibly defined after the class that the
        variable is a member of).'''
        print(f'''{name_of(self.cl)}.{self.name} = {self.expr}''', file=fixup)

class AutoLink(NiceRepr):

    def __init__(self, name, link_spec):
        self.name = name
        self.link_spec = link_spec

    def gen(self, file, fixup):
        my_label = self.link_spec.new_node_port_label
        other_label = self.link_spec.old_node_port_label
        print(Indent(2,
f'''_otherid = self.{my_label}
if _otherid is not None:
    _g.add_edge(_thisid, '{my_label}', _otherid, '{other_label}')'''
        ), file=file, end='')

class Class(NiceRepr):
    '''Generates a Python class for a Node. Provides various methods that
    accumulate elements of the class.'''
    #TODO Make this into an Env, which shadows an enclosing Env

    def __init__(self, name):
        self.name = name
        self.ancestors = []
        self.args = []
        self.inits = []
        self.class_vars = []
        self.auto_links = []
        self.actions = []
        self.suffix = 1  # suffix for generating symbols

    def add_ancestors(self, ancestor):
        self.ancestors += as_iter(ancestor)

    def add_args(self, args):
        self.args += as_iter(args)

    def add_inits(self, inits):
        self.inits += as_iter(inits)

    def add_class_var(self, name_prefix, expr):
        '''Returns name assigned to expr.'''
        name = f'{name_prefix}{self.suffix}'
        self.suffix += 1
        self.class_vars.append(ClassVar(self, name, expr))
        return name

    def add_actions(self, actions):
        '''Each action must support a .gen(file) method.'''
        self.actions += as_iter(actions)

    def add_auto_link(self, lsname, link_spec):
        self.auto_links.append(AutoLink(lsname, link_spec))

    def gen(self, file, fixup):
        print(f'''class {self.name}({self.str_ancestors()}):''', file=file)
        #len1 = file.seek(0, 1)  # length of file so far
        self.gen_class_vars(file, fixup)
        self.gen_init(file, fixup)
        self.gen_auto_links(file, fixup)
        self.gen_actions(file, fixup)
        #if file.seek(0, 1) == len1:  # if body is empty
        #ECCH If file is stdout, then can't test whether anything got printed.
        print('    pass\n', file=file)

    def str_ancestors(self):
        if self.actions:
            last_ancestor = 'ActiveNode'
        else:
            last_ancestor = 'Node'
        return ', '.join(self.ancestors + [last_ancestor])
        
    def gen_class_vars(self, file, fixup):
        for class_var in self.class_vars:
            class_var.gen(file, fixup)

    def gen_init(self, file, fixup):
        if self.args or self.inits:
            #TODO Ideally, we should generate no __init__ function if the
            #Class has no args not shared by its ancestors and no
            #initializers.
            inargs = ', '.join(f'{a}=None' for a in self.args)
            absorb = '\n'.join(f"        kwargs['{a}'] = {a}"
                                   for a in self.args)
            print(f'''
    def __init__(self, {inargs}, **kwargs):
{absorb}
        super().__init__(**kwargs)''', file=file)
            for init in self.inits:
                print(init, file=file)

    def gen_auto_links(self, file, fixup):
        if self.auto_links:
            print(f'''
    def auto_link(self, _thisid, _g):''', file=file)
            for auto_link in self.auto_links:
                auto_link.gen(file, fixup)
            
    def gen_actions(self, file, fixup):
        if self.actions:
            print('''
    def actions(self, _g, _thisid):
        _result = []''', file=file)
            for action in self.actions:
                print(action, file=file, end='')
            print('''        return _result''', file=file)

    def __str__(self):
        sio = StringIO()
        fixup = StringIO()
        self.gen(sio, fixup)
        print(file=sio)
        fixup.seek(0)
        for line in fixup:
            print(line, file=sio, end='')
        return sio.getvalue()
        

class NodeDefn(EnvItem):
    def __init__(self, name, args, body, ancestors):
        self.name = name
        self.args = args
        self.body = body
        self.ancestors = ancestors  # names, not NodeDefns

    def add_to_env(self, env):
        env.add(self.name, self)

    def gen(self, file, env, fixup):
        cl = Class(self.name)

        cl.add_ancestors(self.ancestors)
        cl.add_args(self.true_args(env))

        for link_spec in self.link_specs(env):
            vname = cl.add_class_var('link_spec', repr(link_spec))
            cl.add_auto_link(vname, link_spec)

        for body_item in self.body:
            body_item.add_to_class(cl)

        cl.gen(file, fixup)

    def link_specs(self, env):
        result = []
        for arg in self.args:
            argdef = env.get(arg)
            if isinstance(argdef, PortLabel):
                result.append(
                    LinkSpec(argdef.unique_mate(env).name, argdef.name)
                )
        return result

    #TODO UT
    def true_args(self, env):
        result = []
        for ancestor in self.ancestors:
            #TODO Report error of ancestor undefined
            #TODO Catch circularity
            result += env[ancestor].true_args(env)
        result += self.args
        return result

class OLDNodeDefn(EnvItem):
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

    def add_to_class(self, cl):
        cl.add_inits(Indent(2, f'''self.{self.name} = {as_expr(self.expr)}'''))

class BuildExpr(NiceRepr):
    def __init__(self, expr):
        self.expr = expr

class VarRef(NiceRepr):
    def __init__(self, name):
        self.name = name

    def as_expr(self):
        return self.name

class FuncCall(NiceRepr):

    def __init__(self, funcname, args):
        self.funcname = funcname
        self.args = args

    def as_expr(self):
        return f'''{self.funcname}({', '.join(as_expr(a) for a in self.args)})'''

class Relop(NiceRepr):
    def __init__(self, lhs, op, rhs):
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

class LetExpr(NiceRepr):
    def __init__(self, name, expr):
        self.name = name
        self.expr = expr

class NodesFinder(NiceRepr):

    def __init__(self):
        self.nodespecs = []
        self.whole_tuple_criteria = ['no_dups']

    def add_item(self, item):
        self.nodespecs += as_iter(item.nodespecs())
        self.whole_tuple_criteria += as_iter(item.whole_tuple_criteria())

    def add_to_class(self, cl):
        if self.nodespecs:
            cl.add_class_var('nodes_finder',
f'''CartesianProduct({', '.join(repr(ns) for ns in self.nodespecs)}, whole_tuple_criterion=TupAnd({', '.join(cr) for cr in self.whole_tuple_criteria}))''')

class SeeDo(NiceRepr):
    def __init__(self, conditions, actions, else_conditions, else_actions):
        self.conditions = conditions
        self.actions = actions
        self.else_conditions = else_conditions
        self.else_actions = else_actions

    #TODO rm
    def body_gen(self, build_specs, actions):
        print('SEE')
        pp(self.conditions)
        pp(self.actions)

    def add_to_class(self, cl):
        pass #TODO

class AgentExpr(NiceRepr):

    def __init__(self, expr):
        self.expr = expr

    #TODO rm
    def body_gen(self, build_specs, actions):
        build_specs.append(
            f"BuildSpec({as_expr(self.expr)}, LinkSpec('agents', 'behalf_of'))"
        )

    def add_to_class(self, cl):
        '''cl is a Class object.'''
        name = cl.add_class_var(
            'build_spec',
            f"BuildSpec({as_name(self.expr)}, LinkSpec('agents', 'behalf_of'))"
        )
        cl.add_actions(Indent(2,
            f"_result.append(self.{name}.maybe_make_build_action(_g, _thisid))"
        ))

class ArgExpr(NiceRepr):

    def __init__(self, argname, expr):
        self.argname = argname
        self.expr = expr

    def as_expr(self):
        return f'''{self.argname}={as_expr(self.expr)}'''


#class SeeDoAccumulator(NiceRepr):
#    
#    def __init__(self):
#        pass
#
#    def add_condition(self, condition):
#        self.conditions.add(condition)
#
#    def 
