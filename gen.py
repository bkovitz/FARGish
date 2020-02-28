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
from util import as_iter, as_name, NiceRepr, newline
from exc import NoUniqueMateError
from NodeParams import NodeParams, AttrParam, MateParam
from Indenting import indent

def as_expr(x):
    if hasattr(x, 'as_expr'):
        return x.as_expr()
    else:
        return str(x)

class XIndent:

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
        return [NodeDefn(as_name(name), args_of(name), body, self.ancestors)
                    for name in self.names]

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

    def __init__(self, cl, name, expr, add_after_class_defns=True):
        self.cl = cl  # a Class object
        self.name = name
        self.expr = expr
        self.add_after_class_defns = add_after_class_defns

    def gen(self, file, fixup):
        '''The generated code installs all class variables into classes only
        after all classes are defined. This enables circular references
        between classes (since the expression for the class variable may
        invoke another class, possibly defined after the class that the
        variable is a member of).'''
        if self.add_after_class_defns:
            f = fixup
            pre = f'{as_name(self.cl)}.'
        else:
            f = file
            pre = ''
        print(f'{pre}{self.name} = {self.expr}', file=f)

class AutoLink(NiceRepr):

    def __init__(self, name, link_spec):
        self.name = name
        self.link_spec = link_spec

    def gen(self, file, fixup):
        my_label = self.link_spec.new_node_port_label
        other_label = self.link_spec.old_node_port_label
        print(
f'''_otherid = self.{my_label}
if _otherid is not None:
    _g.add_edge(_thisid, '{my_label}', _otherid, '{other_label}')''',
        file=file, end='')

class Class(NiceRepr):
    '''Generates a Python class for a Node. Provides various methods that
    accumulate elements of the class.'''
    #TODO Make this into an Env, which shadows an enclosing Env

    def __init__(self, name):
        self.name = name
        self.ancestors = []
        self.params = []  # NodeParam objects
        self.inits = []
        self.class_vars = []
        self.auto_links = []
        self.actions = []
        self.suffix = 1  # suffix for generating symbols

    def add_ancestors(self, ancestor):
        self.ancestors += as_iter(ancestor)

    def add_params(self, env, param_names):
        for param_name in as_iter(param_names):
            #TODO Complain about duplicate param names?
            self.params.append(make_node_param(env, param_name))

    def add_inits(self, inits):
        self.inits += as_iter(inits)

    def add_class_var(
        self, name_prefix, expr, no_suffix=False, add_after_class_defns=True
    ):
        '''Returns name assigned to expr.'''
        if no_suffix:
            name = name_prefix
        else:
            name = f'{name_prefix}{self.suffix}'
            self.suffix += 1
        self.class_vars.append(ClassVar(
            self, name, expr, add_after_class_defns=add_after_class_defns
        ))
        return name

    def add_actions(self, actions):
        '''Each action must support a .gen(file, fixup) method. Text that the
        action sends to the 'file' object will go inside the 'def actions()'
        method of the class being generated.'''
        self.actions += as_iter(actions)

    def add_auto_link(self, lsname, link_spec):
        self.auto_links.append(AutoLink(lsname, link_spec))

    def gen(self, file, fixup):
        print(f'''class {self.name}({self.str_ancestors()}):''', file=file)
        file.wrote_any = False
        with indent(file):
            #len1 = file.seek(0, 1)  # length of file so far
            self.add_class_var(
                'node_params',
                repr(NodeParams(*self.params)),
                no_suffix=True,
                add_after_class_defns=False
            )
            self.gen_class_vars(file, fixup)
            self.gen_init(file, fixup)
            #self.gen_auto_links(file, fixup)
            self.gen_actions(file, fixup)
            self.gen_display_name(file, fixup)
            if not file.wrote_any:
                print('pass\n', file=file)
            else:
                print(file=file)

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
        return #TODO rm the rest
        if self.params or self.inits:
            #TODO Ideally, we should generate no __init__ function if the
            #Class has no params not shared by its ancestors and no
            #initializers.
            inargs = ', '.join(f'{a}=None' for a in self.args)
            absorb = '\n'.join(f"kwargs['{a}'] = {a}"
                                   for a in self.args)
            print(f'def __init__(self, {inargs}, **kwargs):', file=file)
            with indent(file):
                print(absorb, file=file)
                print('super().__init__(**kwargs)', file=file)
                for init in self.inits:
                    print(init, file=file)

#    def gen_auto_links(self, file, fixup):
#        if self.auto_links:
#            print(f'''
#    def auto_link(self, _thisid, _g):''', file=file)
#            for auto_link in self.auto_links:
#                auto_link.gen(file, fixup)
            
    def gen_actions(self, file, fixup):
        if self.actions:
            print('def actions(self, _g, _thisid):', file=file)
            with indent(file):
                print('_result = []', file=file)
                for action in self.actions:
                    print(action, file=file)
                print('return _result', file=file)

    def gen_display_name(self, file, fixup):
        #HACK Should display all non-neighbor args
        if len(self.params) == 1:
            print('def display_name(self, g, thisid):', file=file)
            with indent(file):
                print(f"return '{self.name}(' + str(self.{as_name(self.params[0])}) + ')'", file=file)

    def __str__(self):
        sio = StringIO()
        fixup = StringIO()
        self.gen(sio, fixup)
        print(file=sio)
        fixup.seek(0)
        for line in fixup:
            print(line, file=sio, end='')
        return sio.getvalue()
        
def make_node_param(env, name):
    defn = env.get(name)
    if isinstance(defn, PortLabel):
        return MateParam(name, defn.unique_mate(env))
    else:
        # TODO Throw error if name is defined as something inappropriate
        return AttrParam(name)

class NodeDefn(EnvItem):

    def __init__(self, name, param_names, body, ancestors):
        self.name = name
        self.param_names = param_names
        self.body = body
        self.ancestors = ancestors  # names, not NodeDefns

    def add_to_env(self, env):
        env.add(self.name, self)

    def gen(self, file, env, fixup):
        cl = Class(self.name)

        cl.add_ancestors(self.ancestors)
        cl.add_params(env, self.true_param_names(env))

#        for link_spec in self.link_specs(env):
#            vname = cl.add_class_var('link_spec', repr(link_spec))
#            cl.add_auto_link(vname, link_spec)

        for body_item in self.body:
            body_item.add_to_class(cl)

        cl.gen(file, fixup)

#    def link_specs(self, env):
#        result = []
#        for arg in self.args:
#            argdef = env.get(arg)
#            if isinstance(argdef, PortLabel):
#                result.append(
#                    LinkSpec(argdef.unique_mate(env).name, argdef.name)
#                )
#        return result

    #TODO UT
    def true_param_names(self, env):
        result = []
        for ancestor in self.ancestors:
            #TODO Report error of ancestor undefined
            #TODO Catch circularity
            result += env[ancestor].true_param_names(env)
        result += self.param_names
        return result

class Initializer(NiceRepr):

    def __init__(self, name, expr):
        self.name = name
        self.expr = expr

    def add_to_class(self, cl):
        cl.add_inits(f'''self.{self.name} = {as_expr(self.expr)}''')

class Expr(NiceRepr):
    '''as_expr() should generate valid Python code for this expression.'''
    pass

class BuildExpr(Expr):

    def __init__(self, expr):
        self.expr = expr

class VarRef(Expr):

    def __init__(self, name):
        self.name = name

    def as_expr(self):
        return self.name

class FuncCall(Expr):

    def __init__(self, funcname, args):
        self.funcname = funcname
        self.args = args

    def as_expr(self):
        return f'''{self.funcname}({', '.join(as_expr(a) for a in self.args)})'''

class Relop(Expr):
    def __init__(self, lhs, op, rhs):
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

class LetExpr(Expr):
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

class ConditionsActions(NiceRepr):

    def __init__(self, conditions, actions):
        '''conditions: a list (possibly empty) of Exprs and/or ListExprs.
        actions: a list (with at least one element) of Exprs and/or
        BuildExprs.'''
        self.conditions = conditions
        self.actions = actions
        
class SeeDo(NiceRepr):
    def __init__(self, cas):
        '''cas is a list of ConditionsActions objects. After the first, each
        is to be performed only if the previous one did not yield an Action
        (i.e. implementing an 'else').'''
        self.cas = cas

    def add_to_class(self, cl):
        pass #TODO Make an ActionGen and then call .add_to_class on it.

class ActionGen(NiceRepr):
    def __init__(self):
        self.simple_conditions = []
        self.whole_tuple_conditions = []

    def add_simple_condition(self, condition):
        self.simple_conditions += as_iter(condition)

    def add_whole_tuple_condition(self, condition):
        self.whole_tuple_conditions += as_iter(condition)

    def add_to_class(self, cl):
        num_node_searches = sum(
            c.num_node_searches() for c in self.simple_conditions
        )
        if num_node_searches <= 1:
            pass #TODO
        else:
            # generate _tup = CartesianProduct(condition exprs, whole).see_one(g)
            pass #TODO


class AgentExpr(NiceRepr):

    def __init__(self, expr):
        self.expr = expr

    def add_to_class(self, cl):
        '''cl is a Class object.'''
        name = cl.add_class_var(
            'build_spec',
            f"BuildSpec({as_name(self.expr)}, LinkSpec('agents', 'behalf_of'))"
        )
        cl.add_actions(
            f"_result.append(self.{name}.maybe_make_build_action(_g, _thisid))"
        )

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
