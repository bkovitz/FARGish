# gen.py -- Classes holding 'raw' parsed elements of FARGish, generated
#           directly by the syntactic analyzer in grammar.py
#
# These classes, in turn, can generate Python code.

from abc import ABC, abstractmethod
from pprint import pprint as pp
from io import StringIO
from collections import namedtuple

from Env import EnvItem
from LinkSpec import LinkSpec
from NodeSpec import BuildSpec
from util import as_iter, as_name, NiceRepr, newline
from exc import NoUniqueMateError
from NodeParams import NodeParams, AttrParam, MateParam
from Indenting import Indenting, indent

def as_expr(x):
    if hasattr(x, 'as_expr'):
        return x.as_expr()
    else:
        return str(x)

def as_varref(x):
    if hasattr(x, 'as_varref'):
        return x.as_varref()
    else:
        return str(x)

def gen(o, file, fixup, env):
    if hasattr(o, 'gen'):
        o.gen(file, fixup, env)
    else:
        print(str(o), file=file)

def cartprod_elem_name(o, env):
    if hasattr(o, 'cartprod_elem_name'):
        return o.cartprod_elem_name(env)
    else:
        pass

def cartprod_elem_expr(o, env):
    if hasattr(o, 'cartprod_elem_expr'):
        return o.cartprod_elem_expr(env)
    else:
        pass

def is_nodesearch(o, env):
    if hasattr(o, 'is_nodesearch'):
        return o.is_nodesearch(env)
    else:
        return False

def gen_prelines(o, file, fixup, env):
    if hasattr(o, 'gen_prelines'):
        o.gen_prelines(file, fixup, env)
    else:
        pass

class Type: pass
class NodeidT(Type): pass
class NodeSearchT(Type): pass
class ActionT(Type): pass
class PortLabelT(Type): pass
class NodeClassT(Type): pass
class ArbitraryT(Type): pass

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

    def as_varref(self):
        # TODO Is this right in general?
        return f"_g.neighbor(_thisid, {repr(self.name)})"

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

    def gen(self, file, fixup, env):
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

#TODO rm?
class AutoLink(NiceRepr):

    def __init__(self, name, link_spec):
        self.name = name
        self.link_spec = link_spec

    def gen(self, file, fixup, env):
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

    def add_action(self, actions):
        '''Each action must support a .gen(file, fixup, env) method. Text that the
        action sends to the 'file' object will go inside the 'def actions()'
        method of the class being generated.'''
        self.actions += as_iter(actions)

    def add_auto_link(self, lsname, link_spec):
        self.auto_links.append(AutoLink(lsname, link_spec))

    def gen(self, file, fixup, env):
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
            self.gen_class_vars(file, fixup, env)
            self.gen_init(file, fixup, env)
            #self.gen_auto_links(file, fixup, env)
            self.gen_actions(file, fixup, env)
            self.gen_display_name(file, fixup, env)
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
        
    def gen_class_vars(self, file, fixup, env):
        for class_var in self.class_vars:
            gen(class_var, file, fixup, env)

    def gen_init(self, file, fixup, env):
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
                    gen(init, file, fixup, env)

#    def gen_auto_links(self, file, fixup, env):
#        if self.auto_links:
#            print(f'''
#    def auto_link(self, _thisid, _g):''', file=file)
#            for auto_link in self.auto_links:
#                auto_link.gen(file, fixup, env)
            
    def gen_actions(self, file, fixup, env):
        if self.actions:
            print('def actions(self, _g, _thisid):', file=file)
            with indent(file):
                print('_result = []', file=file)
                for action in self.actions:
                    gen(action, file, fixup, env)
                print('return _result', file=file)

    def gen_display_name(self, file, fixup, env):
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

    def as_expr(self):
        return self.name

    def gen(self, file, fixup, env):
        cl = Class(self.name)

        cl.add_ancestors(self.ancestors)
        cl.add_params(env, self.true_param_names(env))

#        for link_spec in self.link_specs(env):
#            vname = cl.add_class_var('link_spec', repr(link_spec))
#            cl.add_auto_link(vname, link_spec)

        for body_item in self.body:
            body_item.add_to_class(cl, env)

        cl.gen(file, fixup, env)

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

    def add_to_class(self, cl, env):
        cl.add_inits(f'''self.{self.name} = {as_expr(self.expr)}''')

class Expr(EnvItem):
    '''as_expr() should generate valid Python code for this expression.'''

    def name(self, env):
        pass

#class BuildExpr(Expr):
#    '''A BuildExpr can never be a condition, only an action.'''
#
#    def __init__(self, expr):
#        self.expr = expr
#
#    def action_condition_expr_gen(env, self):
#        return BuildExprGen(

class SeeDoElem(EnvItem, NiceRepr):

#    @abstractmethod
    def condition_prelines(self, env):
        pass

#    @abstractmethod
    def cartprod_elem_expr(self, env):
        pass

#    @abstractmethod
    def cartprod_elem_name(self, env):
        pass

#    @abstractmethod
    def cartprod_whole_tuple_condition(self, env):
        pass

#    @abstractmethod
    def condition_expr(self, env):
        pass

#    @abstractmethod
    def action_prelines(self, env):
        pass

#    @abstractmethod
    def action_expr(self, env):
        pass

class OLDConditionExpr(SeeDoElem):
    '''Holds zero or more condition expressions in a SeeDo. More than one
    means that they should be ANDed together.'''
    def __init__(self, expr=None):
        if expr is None:
            self.exprs = []
        else:
            self.exprs = [expr]

    #TODO rm
    def add_condition_expr(self, condition_expr):
        self.exprs += condition_expr.exprs

    #TODO rm
    def condition_gen(self):
        #TODO multiple conditions, node searches -> CartesianProduct
        if not len(self.exprs):
            return LocalGen()
        else:
            raise NotImplementedError(self)

class ConditionExpr(SeeDoElem):

    def __init__(self, expr):
        self.expr = expr

    def add_to_env(self, env):
        self.expr.add_to_env(env)

    def cartprod_elem_name(self, env):
        return cartprod_elem_name(self.expr, env)

    def cartprod_elem_expr(self, env):
        return cartprod_elem_expr(self.expr, env)

    def condition_expr(self, env):
        return self.expr.condition_expr(env)

class ActionExpr(SeeDoElem):

    def __init__(self, expr):
        self.expr = expr

    def add_to_env(self, env):
        self.expr.add_to_env(env)

    def action_expr(self, env):
        return self.expr

    def condition_expr(self, env):
        pass  #TODO

    #TODO rm
    def OLDcondition_expr(self):
        '''Returns ConditionExpr to be checked in generated code before
        performing action.'''
        return ConditionExpr()

    #TODO rm
    def action_gen(self):
        return LocalGen(expr=as_expr(self.expr))

class BuildSpecExpr(ActionExpr):

    def __init__(self, nodeclass_expr, args):
        self.nodeclass_expr = nodeclass_expr  # a NodeclassExpr
        self.args = args  # a list of ArgExprs

    def add_to_env(self, env):
        self.nodeclass_expr.add_to_env(env)
        for arg in self.args:
            arg.add_to_env(env)

    def cartprod_whole_tuple_condition(self, env):
        return f"NotAlreadyBuilt({as_expr(self.nodeclass_expr)})"

    def action_expr(self, env):
        cls = as_expr(self.nodeclass_expr)
        args = []
        kwargs = {}
        for arg in self.args:
            #TODO don't reach inside ArgExpr so much
            if not arg.argname:
                args.append(as_expr(arg))
            else:
                kwargs[arg.argname] = as_expr(arg.expr)
        return self.build2(cls, args, kwargs)

    def build2(self, cls, args, kwargs):
        args_s = '[' + ', '.join(args) + ']'
        kwargs_s = \
            '{' + ', '.join(f"{repr(k)}: {v}" for k,v in kwargs.items()) + '}'
        return f"Build2({cls}, args={args_s}, kwargs={kwargs_s})"

    def as_agent_expr(self):
        return AgentExpr(self.nodeclass_expr, self.args)

    #TODO rm
    def condition_gen(self):
        #TODO
        # Make LocalGen that makes a _kwargs variable.
        # Store it here.
        pass

    #TODO rm
    def action_gen(self):
        # TODO  f"Build({as_expr(self.nodeclass_expr)}, kwargs= ???)"
        pass
#        LocalGen(
#            prelines= 

class NodeclassExpr(Expr):

    def __init__(self, name):
        self.name = name

    def add_to_env(self, env):
        #TODO Raise error if self.name is not defined as a nodeclass
        pass

    def as_expr(self):
        return self.name

class VarRef(Expr):

    def __init__(self, name):
        self.name = name
        self.o = None

    def add_to_env(self, env):
        #print('VARENV', self.name, env.get(self.name))
        #TODO Raise error if undefined
        self.o = env.get(self.name)

    def as_expr(self):
        return as_varref(self.o)

    def action_expr_gen(self):
        return self

class Constant(Expr):

    def __init__(self, n):
        self.n = n

    def add_to_env(self, env):
        pass

    def as_expr(self):
        return repr(self.n)

    def action_expr_gen(self):
        return self

class FuncCall(Expr):

    def __init__(self, funcname, args):
        self.funcname = funcname
        self.args = args

    def add_to_env(self, env):
        #TODO Check that self.funcname really refers to a function
        #TODO determine type?
        for arg in self.args:
            arg.add_to_env(env)

    def is_nodesearch(self, env):
        return self.funcname == 'NodeWithTag'  # HACK

    def as_expr(self):
        return f'''{self.funcname}({', '.join(as_expr(a) for a in self.args)})'''

    def action_expr_gen(self):
        return self

class MemberChain(Expr):
    '''An expression containing one or more "dots", like:
          target.value
    '''
    def __init__(self, items):
        self.items = items

    def add_to_env(self, env):
        pass #TODO

    def as_expr(self):
        return '.'.join(self.items)

    #TODO UT
    def action_expr_gen(self):
        return self

class Relexpr(Expr):
    def __init__(self, lhs, op, rhs):
        self.lhs = lhs
        self.op = op
        self.rhs = rhs

    def add_to_env(self, env):
        #TODO Set type to Boolean?
        self.lhs.add_to_env(env)
        self.rhs.add_to_env(env)

    def as_expr(self):
        lhs = as_expr(self.lhs)
        op = as_expr(self.op)
        rhs = as_expr(self.rhs)
        return f"{lhs} {op} {rhs}"

    def condition_expr(self, env):
        return as_expr(self)

    #TODO UT
    def action_expr_gen(self):
        return self

class LetExpr(Expr, SeeDoElem):

    def __init__(self, name, expr):
        self.name = name
        self.expr = expr

    def as_expr(self):
        return self.name

    def add_to_env(self, env):
        self.expr.add_to_env(env)
        #TODO Set type?
        env.add(self.name, self.expr)

    def cartprod_elem_name(self, env):
        if is_nodesearch(self.expr, env):
            return self.name

    def cartprod_elem_expr(self, env):
        if is_nodesearch(self.expr, env):
            return self.expr

class NodesFinder(NiceRepr):

    def __init__(self):
        self.nodespecs = []
        self.whole_tuple_criteria = ['no_dups']

    def add_item(self, item):
        self.nodespecs += as_iter(item.nodespecs())
        self.whole_tuple_criteria += as_iter(item.whole_tuple_criteria())

    def add_to_class(self, cl, env):
        if self.nodespecs:
            cl.add_class_var('nodes_finder',
f'''CartesianProduct({', '.join(repr(ns) for ns in self.nodespecs)}, whole_tuple_criterion=TupAnd({', '.join(cr) for cr in self.whole_tuple_criteria}))''')

#class ConditionWithActions(NiceRepr):
#
#    def __init__(self, condition, actions):
#        '''condition: a ConditionExpr.
#        actions: a list (with at least one element) of ActionExprs.'''
#        self.condition = condition
#        self.actions = actions
#
#    def add_to_seedogen(self, see_do_gen):
#        see_do_gen.add_condition_gen(self.condition.condition_gen())
#        for action in self.actions:
#            see_do_gen.add_condition_gen(action.condition_gen())
#            see_do_gen.add_action_gen(action.action_gen())

ConditionWithActions = namedtuple('ConditionWithActions', [
    'condition',  # a ConditionExpr (holding zero or more conditions)
    'actions'     # a list of at least one ActionExpr
])
        
#ConditionsWithActions = namedtuple('ConditionsWithActions', [
#    'conditions',  # a list of zero or more ConditionExprs
#    'actions'     # a list of at least one ActionExpr
#])

class ConditionsWithActions(NiceRepr):

    def __init__(self, conditions, actions):
        '''conditions: a list of zero or more ConditionExprs
           actions:    a list of at least one ActionExpr'''
        self.conditions = conditions
        self.actions = actions

    def make_gen(self, env, more):
        '''Returns an object suitable for calling from Class.gen_actions()
        to generate code to perform any specified or implied node searches,
        check conditions, and append an Action object to _result.

        'more' is a list of zero or more ConditionsWithActions to generate
        inside successive nested 'else' clauses.'''
        condition_prelines = []
        cartprod_elems = []
        cartprod_names = []
        whole_tuple_conditions = ['no_dups']
        condition_exprs = []
        action_prelines = []
        action_exprs = []

        env.push()

        for condition in self.conditions:
            print('MAKEC', condition)
            condition.add_to_env(env)
            condition_prelines += as_iter(condition.condition_prelines(env))
            cartprod_elems += as_iter(condition.cartprod_elem_expr(env))
            cartprod_names += as_iter(condition.cartprod_elem_name(env))
            whole_tuple_conditions += \
                as_iter(condition.cartprod_whole_tuple_condition(env))
            condition_exprs += as_iter(condition.condition_expr(env))

        #print('CONDS', self.conditions)
        #print(condition_exprs)

        for action in self.actions:
            action.add_to_env(env)
            condition_prelines += as_iter(action.condition_prelines(env))
            cartprod_elems += as_iter(action.cartprod_elem_expr(env))
            cartprod_names += as_iter(action.cartprod_elem_name(env))
            whole_tuple_conditions += \
                as_iter(action.cartprod_whole_tuple_condition(env))
            condition_exprs += as_iter(action.condition_expr(env))
            action_prelines += as_iter(action.action_prelines(env))
            action_exprs += as_iter(action.action_expr(env))

        nextgen = None
        if more:
            nextgen = more[0].make_gen(env, more[1:])

        def genfunc(file, fixup, ignored_env):
            if cartprod_names:
                print(' = '.join(nm for nm in cartprod_names + ['None']),
                      file=file)
            for preline in condition_prelines:
                print(preline, file=file)
            if cartprod_elems:
                found_tup = env.gensym('_found_tup')
                tup_elems = ', '.join(as_expr(e) for e in cartprod_elems)
                whole_crit = f"TupAnd({', '.join(whole_tuple_conditions)})"
                print(f"{found_tup} = CartesianProduct({tup_elems}, whole_tuple_criterion={whole_crit}).see_one(_g)", file=file)
                print(f"if {found_tup}:", file=file)
                with indent(file):
                    if len(cartprod_elems) == 1:
                        print(f"({cartprod_names[0]},) = {found_tup}",
                            file=file)
                    else:
                        print(' = '.join(nm for nm in
                            cartprod_names + [found_tup]), file=file)
                condition_exprs.insert(0, found_tup)
            if condition_exprs:
                print(f"if {' and '.join(condition_exprs)}:", file=file)
                with indent(file):
                    self.gen_actions(action_prelines, action_exprs, file)
                if nextgen:
                    print('else:', file=file)
                    with indent(file):
                        gen(nextgen, file, fixup, ignored_env)
            else:
                self.gen_actions(action_prelines, action_exprs, file)
                if nextgen:
                    nextgen(file, fixup, ignored_env)

        #print('GEN ENV', str(env))
        env.pop()

        return LocalGen(genfunc)

        #return s.getvalue() #TODO a 'gen' function

    @classmethod
    def gen_actions(cls, action_prelines, action_exprs, file):
        for action_preline in action_prelines:
            print(action_preline, file=file)
        for action_expr in action_exprs:
            print(f"_result.append({as_expr(action_expr)})", file=file)
        
class SeeDo(NiceRepr):
    def __init__(self, cas):
        '''cas is a list of ConditionWithActions objects. After the first, each
        is to be performed only if the previous one did not yield an Action
        (i.e. implementing an 'else').'''
        self.cas = cas

    def add_to_class(self, cl, env):
        if self.cas:
            cl.add_action(self.cas[0].make_gen(env, self.cas[1:]))

        return #TODO rm the rest of this
        first_sdg = sdg = None
        for ca in self.cas:
            if sdg is None:
                first_sdg = sdg = SeeDoGen()
            else:
                sdg = sdg.start_else()

            # Add implicit preconditions for actions
            print('CA', ca)
            for action_expr in ca.actions:
                ca.condition.add_condition_expr(action_expr)

            sdg.add_condition_gen(ca.condition.condition_gen())
            for action in ca.actions:
                sdg.add_condition_gen(action.condition_gen())
                sdg.add_action_gen(action.action_gen())
            #ca.add_to_seedogen(sdg)  #TODO rm
        cl.add_action(first_sdg)

#class CodeGenerator(ABC, NiceRepr):
#
#    @abstractmethod
#    def gen(self, file, fixup, env):
#        '''Generate code represented by this object.'''
#        pass
#
#class NullConditionGen(CodeGenerator):
#
#    def gen(self, file, fixup, env):
#        pass
#
#class ActionGen(CodeGenerator):
#
#    @abstractmethod
#    def add_action(self, action):
#        pass
#
#class NullActionGen(ActionGen):
#
#    def gen(self, file, fixup, env):
#        pass
#
#    def add_action(self, action):
#        return action.as_action_gen()


class LocalGen(NiceRepr):
    
    def __init__(self, func):
        '''func(file, fixup, env) is a code-generation function. The LocalGen
        object is suitable for passing to the global gen() function.'''
        self.gen = func

class OLDLocalGen(NiceRepr):
    '''Code generator for an expression that *might* require defining
    named variables and hence multiple lines of code. Caller must call
    .gen_prelines() before .final_expr(), since the former might store
    names in .dict that are read by the latter.'''

    def __init__(self, prelines=None, expr=''):
        if prelines is None:
            prelines = []
        self._prelines = prelines
        self.expr = expr
        self.dict = {}  # Holds variables that can be read by .final_expr()

    def gen_prelines(self, file, fixup, env):
        for preline in self._prelines:
            gen(preline, file, fixup, env)

    def as_expr(self):
        '''Returns a Python expression, possibly referring to variables
        defined by code generated by .gen_prelines().'''
        return self.final_expr()

    def final_expr(self):
        return self.expr

    def add_preline(self, line):
        self._prelines += as_iter(line)

#TODO rm this whole class?
class SeeDoGen(NiceRepr):
    '''Generates all the Python code to implement a SeeDo.'''

    def __init__(self):
        self.condition_gens = []  # list of ExprGens
        self.action_gens = []     # list of ExprGens
        self.else_action_gen = None

    def add_condition_gen(self, condition_gen):
        '''condition_gen: an LocalGen for a condition in this SeeDoGen, or
        None.'''
        if condition_gen:
            self.condition_gens.append(condition_gen)

    def add_action_gen(self, action_gen):
        '''condition_gen: an LocalGen for an action in this SeeDoGen, or
        None.'''
        if action_gen:
            self.action_gens.append(action_gen)

    def start_else(self):
        self.else_action_gen = SeeDoGen()
        return self.else_action_gen

    def gen(self, file, fixup, env):
        # If more than one node search, make a CartesianProduct and a little
        # function to assign names to elements of the tuple.
        # Otherwise generate prelines and if_stmt.
        self.gen_conditions(file, fixup, env)
        made_if = self.gen_if_stmt(file, fixup, env)
        if made_if:
            with indent(file):
                self.gen_actions(file, fixup, env)
        else:
            self.gen_actions(file, fixup, env)

    def gen_conditions(self, file, fixup, env):
        for condition_gen in self.condition_gens:
            gen_prelines(condition_gen, file, fixup, env)

    def gen_if_stmt(self, file, fixup, env):
        '''Returns True iff generated an 'if' statement.'''
        print('GEN_IF', self.condition_gens)
        exprs = [as_expr(cg) for cg in self.condition_gens]
        e = ' and '.join(expr for expr in exprs if expr)
        if e:
            print(f'if {e}:', file=file)
            return True
        else:
            return False

    def gen_actions(self, file, fixup, env):
        for action_gen in self.action_gens:
            gen_prelines(action_gen, file, fixup, env)
            expr = as_expr(action_gen)
            if expr:
                print(f'_result.append({expr})', file=file)
        if self.else_action_gen:
            with indent(file):
                print('else:', file=file)
                self.else_action_gen.gen(file, fixup, env)

#        num_node_searches = sum(
#            c.num_node_searches() for c in self.simple_conditions
#        )
#        if num_node_searches <= 1:
#            pass #TODO
#        else:
#            # generate _tup = CartesianProduct(condition exprs, whole).see_one(g)
#            pass #TODO


class OLDAgentExpr(NiceRepr):

    def __init__(self, expr):
        self.expr = expr

    def add_to_env(self, env):
        pass #TODO

    def add_to_class(self, cl, env):
        '''cl is a Class object.'''
        name = cl.add_class_var(
            'build_spec',
            f"BuildSpec({as_name(self.expr)}, LinkSpec('agents', 'behalf_of'))"
        )
        cl.add_action(
            f"_result.append(self.{name}.maybe_make_build_action(_g, _thisid))"
        )

class AgentExpr(BuildSpecExpr):

    def cartprod_whole_tuple_condition(self, env):
        pass

    def build2(self, cls, args, kwargs):
        if 'behalf_of' not in kwargs:
            kwargs['behalf_of'] = '_thisid'
        args_s = '[' + ', '.join(args) + ']'
        kwargs_s = \
            '{' + ', '.join(f"{repr(k)}: {v}" for k,v in kwargs.items()) + '}'
        return f"Build2.maybe_make(_g, {cls}, args={args_s}, kwargs={kwargs_s}, potential_neighbors=[_thisid])"

class ArgExpr(NiceRepr):

    def __init__(self, argname, expr):
        self.argname = argname
        self.expr = expr

    def add_to_env(self, env):
        self.expr.add_to_env(env)

    def as_expr(self):
        if self.argname:
            return f'''{self.argname}={as_expr(self.expr)}'''
        else:
            return as_expr(self.expr)


#class SeeDoAccumulator(NiceRepr):
#    
#    def __init__(self):
#        pass
#
#    def add_condition(self, condition):
#        self.conditions.add(condition)
#
#    def 
