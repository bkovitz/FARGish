# gen.py -- Classes holding 'raw' parsed elements of FARGish, generated
#           directly by the syntactic analyzer in grammar.py
#
# These classes, in turn, can generate Python code.

from abc import ABC, abstractmethod
from pprint import pprint as pp
from io import StringIO
from collections import namedtuple
import traceback

from Env import EnvItem
from LinkSpec import LinkSpec
#from NodeSpec import BuildSpec
from util import as_iter, as_list, as_name, NiceRepr, newline, filter_none
from exc import NoUniqueMateError
from NodeParams import NodeParams, AttrParam, MateParam
from Indenting import Indenting, indent

def as_pyexpr(x):
    if hasattr(x, 'as_pyexpr'):
        return x.as_pyexpr()
    elif isinstance(x, list) or isinstance(x, tuple):
        return '[' + (', '.join(as_pyexpr(e) for e in x)) + ']'
    else:
        return str(x)

def gen(o, file, fixup, env):
    if hasattr(o, 'gen'):
        o.gen(file, fixup, env)
    else:
        print(str(o), file=file)

#TODO rm?
def cartprod_elem_name(o, env):
    if hasattr(o, 'cartprod_elem_name'):
        return o.cartprod_elem_name(env)
    else:
        pass

#TODO rm?
def cartprod_elem_expr(o, env):
    if hasattr(o, 'cartprod_elem_expr'):
        return o.cartprod_elem_expr(env)
    else:
        pass

#TODO rm?
nodesearch_names = {'NodeWithTag', 'NodeOfClass', 'NodeSpec'} #HACK

#TODO rm?
def is_nodesearch(o, env):
    if hasattr(o, 'is_nodesearch'):
        return o.is_nodesearch(env)
    else:
        return False

#TODO rm?
def gen_prelines(o, file, fixup, env):
    if hasattr(o, 'gen_prelines'):
        o.gen_prelines(file, fixup, env)
    else:
        pass

class CallGenerator(ABC):
    '''Mix-in for items that can represent functions to call from FARGish.'''

    @abstractmethod
    def pycall(self, argexprs):
        '''Should return a string containing appropriate Python code to call
        whatever 'self' represents.'''
        pass

class Type: pass
# class NodeidT(Type): pass
# class NodeSearchT(Type): pass
# class ActionT(Type): pass
# class PortLabelT(Type): pass
# class NodeClassT(Type): pass
# class ArbitraryT(Type): pass
class GFuncT(Type): pass  # A function with "_g" as implicit first arg
class Func(Type): pass    # A function with no implicit args

# class ExternalName(EnvItem):
#     def __init__(self, name):
#         self.name = name
# 
#     def add_to_env(self, env):
#         env.add(self.name, self)
#         
# class ExternalList(EnvItem):
#     def __init__(self, names):
#         '''names: a list of external function names'''
#         self.names = [ExternalName(name) for name in names]
# 
#     def add_to_env(self, env):
#         for name in self.names:
#             name.add_to_env(env)

class PortLabel(EnvItem, CallGenerator):
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

    def pycall(self, argexprs):
        assert len(argexprs) == 1  # HACK Should raise a compilation error
        nodeexpr = argexprs[0]
        return f"_g.neighbors({as_pyexpr(nodeexpr)}, port_label={repr(self.name)})"

class PortLabelParent(EnvItem):

    def __init__(self, child, parent):
        self.child = child
        self.parent = parent

    def add_to_env(self, env):
        pass

class LinkDefn(EnvItem):

    def __init__(self, from_label, to_label):
        self.from_label = PortLabel(from_label)
        self.to_label = PortLabel(to_label)

    def add_to_env(self, env):
        plfrom = env.get_or_add(self.from_label.name, self.from_label)
        plto = env.get_or_add(self.to_label.name, self.to_label)
        plfrom.add_link_to(plto)
        plto.add_link_to(plfrom)

class ExtFunc(CallGenerator, EnvItem):
    '''An external function, i.e. a Python function accessible from FARGish.'''

    def __init__(self, name):
        self.name = name

    def add_to_env(self, env):
        env.add(self.name, self)

    def pycall(self, argexprs):
        pyargs = ', '.join(as_pyexpr(a) for a in argexprs)
        return f'''{self.name}({pyargs})'''

class ExtGFunc(ExtFunc):
    '''An external function that takes the current PortGraph as its first
    argument. This class supplies "_g" as that argument implicitly, so it
    doesn't need to be specified in FARGish code.'''

    def pycall(self, argexprs):
        pyargs = ', '.join(['_g'] + [as_pyexpr(a) for a in argexprs])
        return f'''{self.name}({pyargs})'''
        

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

class Class:
    '''Generates a Python class for a Node. Provides various methods that
    accumulate elements of the class.'''
    #TODO Make this into an Env, which shadows an enclosing Env

    def __init__(self, name):
        self.name = name
        self.ancestors = []
        self.params = []  # NodeParam objects
        self.inits = []
        self.class_vars = []
        self.actions = []
        self.suffix = 1  # suffix for generating symbols
        if self.name == 'Tag':
            self.add_class_var(
                'is_tag', 'True', no_suffix=True, add_after_class_defns=False
            )

    def add_ancestors(self, ancestor):
        self.ancestors += as_iter(ancestor)

    def add_params(self, env, param_names):
        for param_name in as_iter(param_names):
            #if not any(param.as_key() == param_name for param in self.params):
            #Skipping duplicate parameters doesn't work because sometimes
            #we want multiple parameters with the same name.
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

    def gen(self, file, fixup, env):
        print(f'''class {self.name}({self.str_ancestors()}):''', file=file)
        file.wrote_any = False
        with indent(file):
            self.add_class_var(
                'node_params',
                repr(NodeParams(*self.params)),
                no_suffix=True,
                add_after_class_defns=False
            )
            self.gen_class_vars(file, fixup, env)
            self.gen_init(file, fixup, env)
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
        pyexprs = filter_none(lambda p: p.display_name_pyexpr(), self.params)
        if pyexprs:
            print('def display_name(self, _g, _thisid):', file=file)
            s = ', '.join(f"{{{p}}}" for p in pyexprs)
            with indent(file):
                print(#f"return '{self.name}(' + {s} + ')'",
                      f"return f\"{self.name}({s})\"",
                      file=file)

    def __repr__(self):
        return f"Class({repr(self.name)})"

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

    def as_pyexpr(self):
        return self.name

    def gen(self, file, fixup, env):
        cl = Class(self.name)

        cl.add_ancestors(self.ancestors)
        cl.add_params(env, self.true_param_names(env))

        for body_item in self.body:
            body_item.add_to_class(cl, env)

        cl.gen(file, fixup, env)

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
        cl.add_inits(f'''self.{self.name} = {as_pyexpr(self.expr)}''')

class Expr(EnvItem):
    '''as_pyexpr() should generate valid Python code for this expression.'''

    @abstractmethod
    def gen_prelines(self, file, fixup, env):
        '''Generates any lines of code that must appear before the line
        containing the Python expression.'''
        pass

    def gen_prelines2(self, file, fixup, env):
        pass #STUB
        # if nodesearches
        #    if whole_tuple_criterion
        #        define a function for it
        #    make a variable for the tup
        #    assign it a CartesianProduct
        # if names defined
        #    predefine them to None

        # IDEA Convert self into a CartProdExpr if needed

    @abstractmethod
    def as_pyexpr(self):  # Don't we need to pass env?
        pass

    @abstractmethod
    def coalesced_with(self, other):
        pass

NodeSearch = namedtuple('NodeSearch', ('name', 'expr'))

class CartProdExpr(Expr):

    def __init__(self, nodesearches, whole_tuple_criteria):
        self.nodesearches = nodesearches  # list of NodeSearch objects
            # A CartProdExpr must have at least one NodeSearch
        self.whole_tuple_criteria = whole_tuple_criteria # list of Exprs
        self.func_name = None
        self.tup_name = None

    def add_to_env(self, env):
        for ns in self.nodesearches:
            ns.expr.add_to_env(env)
        for wtc in self.whole_tuple_criteria:
            wtc.add_to_env(env)

    def gen_prelines(self, file, fixup, env):
        self.tup_name = env.gensym('_found_tup')
        #print(f"{self.elem_names_comma()} = None", file=file)
        print(' = '.join(self.elem_names() + ['None']), file=file)
        for ns in self.nodesearches:
            ns.expr.gen_prelines(file, fixup, env)
        ns_py = ', '.join(as_pyexpr(ns.expr) for ns in self.nodesearches)
        if self.whole_tuple_criteria:
            self._gen_tupfunc(file, fixup, env)
            crit_py = f"TupFunc({self.func_name})"
            if len(self.nodesearches) != 1:
                crit_py = f"TupAnd(no_dups, {crit_py})"
            wtc_py = f", whole_tuple_criterion={crit_py}"
        else:
            wtc_py = ''
        print(
            f"{self.tup_name} = CartesianProduct({ns_py}{wtc_py}).see_one(_g)",
            file=file
        )
        print(f"if {self.tup_name}:", file=file)
        with indent(file):
            self._gen_unpack(self.tup_name, file, fixup, env)

    def _gen_tupfunc(self, file, fixup, env):
        '''Sets .func_name and generates the definition for the function to
        be passed to TupFunc.'''
        self.func_name = env.gensym('_f')
        print(f"def {self.func_name}(_g, _tup):", file=file)
        with indent(file):
            self._gen_unpack('_tup', file, fixup, env)
            for wtc in self.whole_tuple_criteria:
                wtc.gen_prelines(file, fixup, env)
            wtcs_py = ' and '.join(
                as_pyexpr(wtc) for wtc in self.whole_tuple_criteria
            )
            print(f"return {wtcs_py}", file=file)

    def _gen_unpack(self, tupname, file, fixup, env):
        print(f"{self.elem_names_comma()}, = {tupname}", file=file)
        
    def elem_names_comma(self):
        return ', '.join(self.elem_names())

    def elem_names(self):
        return [ns.name for ns in self.nodesearches]

    def as_pyexpr(self):
        return self.tup_name

    def coalesced_with(self, other):
        if isinstance(other, CartProdExpr):
            return CartProdExpr(
                self.nodesearches + other.nodesearches,
                self.whole_tuple_criteria + other.whole_tuple_criteria
            )
        else:
            return CartProdExpr(
                self.nodesearches, self.whole_tuple_criteria + [other]
            )

class NodeclassExpr(Expr):

    def __init__(self, name):
        self.name = name

    def gen_prelines(self, file, fixup, env):
        pass

    def add_to_env(self, env):
        #TODO Raise error if self.name is not defined as a nodeclass
        pass

    def as_pyexpr(self):
        return self.name

    def coalesced_with(self, other):
        #TODO Does this make any sense?
        return AndExpr(self, other)

class VarRef(Expr):

    def __init__(self, name):
        self.name = name
        self.o = None

    def add_to_env(self, env):
        #print('VARENV', self.name, env.get(self.name))
        #TODO Raise error if undefined
        self.o = env.get(self.name)

    def gen_prelines(self, file, fixup, env):
        pass

    def as_pyexpr(self):
        #TODO Need to insert "self." for attribute variables
        if hasattr(self.o, 'as_varref'):
            return self.o.as_varref()
        else:
            return self.name

    def coalesced_with(self, other):
        return AndExpr(self, other)

    def __repr__(self):
        return f"VarRef({repr(self.name)})"

class ThisExpr(Expr):

    def add_to_env(self, env):
        pass

    def gen_prelines(self, file, fixup, env):
        pass

    def as_pyexpr(self):
        return '_thisid'

    def coalesced_with(self, other):
        return AndExpr(self, other)

class AndExpr(Expr):

    def __init__(self, *exprs):
        #print('AND1', exprs)
        #traceback.print_stack()
        self.exprs = exprs  # TODO Handle null case?

    def add_to_env(self, env):
        for expr in self.exprs:
            expr.add_to_env(env)

    def as_pyexpr(self):
        #TODO Handle null case
        return ' and '.join(as_pyexpr(e) for e in self.exprs)

    def gen_prelines(self, file, fixup, env):
        #print('AND', self.exprs)
        for expr in self.exprs:
            expr.gen_prelines(file, fixup, env)

    def coalesced_with(self, other):
        return AndExpr(*self.exprs, other)

class Constant(Expr):

    def __init__(self, x):
        self.x = x

    def add_to_env(self, env):
        pass

    def as_pyexpr(self):
        return repr(self.x)

    def gen_prelines(self, file, fixup, env):
        pass

    def coalesced_with(self, other):
        return AndExpr(self, other)

class PyLiteralExpr(Expr):
    '''An Expr consisting of a Python expression. A PyLiteralExpr appears in
    generated code as-is, with no modification.'''

    def __init__(self, x):
        self.x = x

    def add_to_env(self, env):
        pass

    def as_pyexpr(self):
        return self.x

    def gen_prelines(self, file, fixup, env):
        pass

    def coalesced_with(self, other):
        return AndExpr(self, other)

class FuncCall(Expr):

    def __init__(self, funcname, args):
        self.funcname = funcname  # TODO Treat funcname as an Expr
        self.args = args
        self.callgen = None

    def add_to_env(self, env):
        #TODO Check that self.funcname really refers to a function
        self.callgen = env.get(self.funcname)
        for arg in self.args:
            arg.add_to_env(env)

    def gen_prelines(self, file, fixup, env):
        for arg in self.args:
            arg.gen_prelines(file, fixup, env)

    def is_nodesearch(self, env):
        return self.funcname in nodesearch_names  # HACK

    def as_pyexpr(self):
        try:
            make_pycall = self.callgen.pycall
        except AttributeError:
            pyargs = ', '.join(as_pyexpr(a) for a in self.args)
            return f'''{self.funcname}({pyargs})'''
        else:
            return make_pycall(self.args)

    def coalesced_with(self, other):
        #TODO Handle nodesearch differently
        return AndExpr(self, other)

class MemberChain(Expr):
    '''An expression containing one or more "dots", like:
          target.value
    '''
    def __init__(self, items):
        self.items = items

    def add_to_env(self, env):
        for item in self.items:
            if hasattr(item, 'add_to_env'):
                item.add_to_env(env)

    def gen_prelines(self, file, fixup, env):
        #HACK
        pass

    def as_pyexpr(self):
        #HACK
        return f"_g.value_of({as_pyexpr(self.items[0])}, attr_name={repr(as_pyexpr(self.items[1]))})"
        #return '.'.join(self.items)

    def coalesced_with(self, other):
        return AndExpr(self, other)

class Relexpr(Expr):

    def __init__(self, lhs, op, rhs):
        self.lhs = lhs  # An Expr
        self.op = op    # A string: '==', '!=', etc.
        self.rhs = rhs  # An Expr

    def add_to_env(self, env):
        #TODO Set type to Boolean?
        self.lhs.add_to_env(env)
        self.rhs.add_to_env(env)

    def gen_prelines(self, file, fixup, env):
        self.lhs.gen_prelines(file, fixup, env)
        self.rhs.gen_prelines(file, fixup, env)

    def as_pyexpr(self):
        lhs = as_pyexpr(self.lhs)
        op = as_pyexpr(self.op)
        rhs = as_pyexpr(self.rhs)
        return f"{lhs} {op} {rhs}"

    def condition_expr(self, env):
        return as_pyexpr(self)

    def coalesced_with(self, other):
        return AndExpr(self, other)

class LetExpr(Expr):

    def __init__(self, name, expr):
        self.name = name
        self.expr = expr

    def as_pyexpr(self):
        return self.name

    def gen_prelines(self, file, fixup, env):
        self.expr.gen_prelines(file, fixup, env)

    def add_to_env(self, env):
        self.expr.add_to_env(env)
        #TODO Set type?
        env.add(self.name, self.expr)

ConditionWithActions = namedtuple('ConditionWithActions', [
    'condition',  # a ConditionExpr (holding zero or more conditions)
    'actions'     # a list of at least one ActionExpr0
])
        
class ConditionsWithActions(NiceRepr):

    def __init__(self, conditions, actions):
        '''conditions: a list of zero or more ConditionExprs
           actions:    a list of at least one ActionExpr0'''
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

        with env:
            for condition in self.conditions:
                #print('MAKEC', condition)
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
                    tup_elems = ', '.join(as_pyexpr(e) for e in cartprod_elems)
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
        #env.pop()

        return LocalGen(genfunc)

    #TODO If we switch to Stmts, change this to let the Stmt gen the
    #"_result.append(...)
    @classmethod
    def gen_actions(cls, action_prelines, action_exprs, file):
        for action_preline in action_prelines:
            print(action_preline, file=file)
        for action_expr in action_exprs:
            print(f"_result.append({as_pyexpr(action_expr)})", file=file)
        
class LocalGen(NiceRepr):
    
    def __init__(self, func):
        '''func(file, fixup, env) is a code-generation function. The LocalGen
        object is suitable for passing to the global gen() function.'''
        self.gen = func

class ArgExpr(NiceRepr):

    def __init__(self, argname, expr):
        self.argname = argname
        self.expr = expr

    def is_positional(self):
        return not self.argname

    def add_to_env(self, env):
        self.expr.add_to_env(env)

    def gen_prelines(self, file, fixup, env):
        self.expr.gen_prelines(file, fixup, env)

    def as_pyexpr(self):
        if self.argname:
            return f'''{self.argname}={as_pyexpr(self.expr)}'''
        else:
            return as_pyexpr(self.expr)

class TupleExpr(Expr):

    def __init__(self, *elem_exprs):
        self.elem_exprs = elem_exprs

    def add_to_env(self, env):
        for elem_expr in self.elem_exprs:
            elem_expr.add_to_env(env)

    def gen_prelines(self, file, fixup, env):
        for elem_expr in self.elem_exprs:
            elem_expr.gen_prelines(file, fixup, env)

    def as_pyexpr(self):
        if not self.elem_exprs:
            return '()'
        elif len(self.elem_exprs) == 1:
            s = as_pyexpr(self.elem_exprs[0])
            return f"({s},)"
        else:
            s = ', '.join(as_pyexpr(e) for e in self.elem_exprs)
            return f"({s})"

    def coalesced_with(self, other):
        return AndExpr(self, other)

class NullConditionExpr(Expr):

    def __init__(self, *args, **kwargs):
        self.args = args  # STUB
        self.kwargs = kwargs  # STUB

    def gen_prelines(self, file, fixup, env):
        pass #TODO

    def add_to_env(self, env):
        pass #STUB

    def as_pyexpr(self):
        return 'True'  # TODO

    def __bool__(self):
        '''Returns True iff the condition can be optimized out, i.e. if
        the condition evaluates to True unconditionally.'''
        return True   # TODO

    def add_condition(self, other):
        pass #TODO

class WholeTupleConditionExpr(Expr):

    def __init__(self):
        self.tupvar = 'TUPVAR_UNINITIALIZED'

    def gen_prelines(self, file, fixup, env):
        #print(f"{found_tup} = CartesianProduct({tup_elems}, whole_tuple_criterion={whole_crit}).see_one(_g)", file=file)
        #print(f"if {found_tup}:", file=file)
        self.tupvar = env.gensym('_found_tup')
        print(f"{self.tupvar} = CartesianProduct(STUB).see_one(_g)", file=file)

    def as_pyexpr(self):
        return self.tupvar

class NotAlreadyBuiltExpr(Expr):
    
    def __init__(self, buildstmt):
        self.buildstmt = buildstmt
        super().__init__()

    def add_to_env(self, env):
        pass

    def gen_prelines(self, file, fixup, env):
        #TODO Do we need to call .gen_prelines on all the args and kwargs
        #or does BuildStmt already do that?
        pass

    def as_pyexpr(self):
        return \
            f"not _g.already_built({self.buildstmt.buildargs_py()})"

    def coalesced_with(self, other):
        return AndExpr(self, other)

class Stmt(EnvItem):

    @abstractmethod
    def implicit_cond_expr(self):
        pass

    @abstractmethod
    def gen(self, file, fixup, env):
        pass

    def coalesced_with(self, stmt):
        return StmtSeq([self, stmt])

    def __bool__(self):
        '''Only the NullStmt should evaluate False.'''
        return True

class ActionStmt(Stmt):

    @abstractmethod
    def action_pyexpr(self):
        '''Return a string or list of strings, each of which is the Python
        expression for an Action.'''
        pass

    def gen(self, file, fixup, env):
        for pye in as_iter(self.action_pyexpr()):
            print(f"_result.append({self.action_pyexpr()})", file=file)

class ActionExpr(ActionStmt):
    '''An Expr to be treated as an ActionStmt.'''

    def __init__(self, expr):
        self.expr = expr

    def add_to_env(self, env):
        self.expr.add_to_env(env)

    def action_pyexpr(self):
        return as_pyexpr(self.expr)

    def gen(self, file, fixup, env):
        self.expr.gen_prelines(file, fixup, env)
        super().gen(file, fixup, env)

    def implicit_cond_expr(self):
        return None

class NullStmt(Stmt):

    def implicit_cond_expr(self):
        return None

    def add_to_env(self, env):
        pass

    def gen(self, file, fixup, env):
        print('pass', file=file)

    def coalesced_with(self, stmt):
        return stmt

    def __bool__(self):
        return False

class StmtSeq(Stmt):
    '''A sequence of statements.'''

    def __init__(self, stmts):
        '''stmts: a list of Stmt objects.'''
        self.stmts = stmts

    def implicit_cond_expr(self):
        return coalesce_conditions(
            (s.implicit_cond_expr() for s in self.stmts), None
        )

    def gen(self, file, fixup, env):
        for stmt in self.stmts:
            stmt.gen(file, fixup, env)

    def coalesced_with(self, stmt):
        self.stmts += as_iter(stmt)

class IfStmt(Stmt):

    def __init__(self, cond_expr, then_expr, else_expr):
        self.cond_expr = coalesce_conditions(cond_expr, then_expr)
        self.then_expr = then_expr
        self.else_expr = else_expr

    def add_to_env(self, env):
        if self.cond_expr:
            self.cond_expr.add_to_env(env)
        if self.then_expr:
            self.then_expr.add_to_env(env)
        if self.else_expr:
            self.else_expr.add_to_env(env)

    def implicit_cond_expr(self):
        return None

    def gen(self, file, fixup, env):
        if self.cond_expr:
            self.cond_expr.gen_prelines(file, fixup, env)
            print(f"if {as_pyexpr(self.cond_expr)}:", file=file)
            with indent(file), env:
                self.gen_then(file, fixup, env)
            if self.else_expr:
                print('else:', file=file)
                with indent(file), env:
                    self.else_expr.gen(file, fixup, env)
        else:
            self.gen_then(file, fixup, env)

    def gen_then(self, file, fixup, env):
        self.then_expr.gen(file, fixup, env)

agent_argexpr = ArgExpr('behalf_of', PyLiteralExpr('_thisid'))

class BuildStmt(ActionStmt):

    def __init__(self, nodeclass_expr, args):
        self.nodeclass_expr = nodeclass_expr  # a NodeclassExpr
        self.args = args  # a list of ArgExprs

    #TODO implicit_cond_expr: make the _b variable to hold the BuildSpec to
    # be referenced in the action_pyexpr.

    def add_to_env(self, env):
        self.nodeclass_expr.add_to_env(env)
        for arg in self.args:
            arg.add_to_env(env)

    def action_pyexpr(self):
        # TODO .gen_prelines the self.args
        return f"make_build3(_g, {self.buildargs_py()})"
        
    def buildargs_py(self):
        args_py = '[' + ', '.join(
            as_pyexpr(a) for a in positional_args(self.args)
        ) + ']'
        kwargs_py = '{' + ', '.join(
                        f"{repr(k)}: {as_pyexpr(v)}"
                            for k,v in keyword_args(self.args)
                    ) + '}'
        cl_py = as_pyexpr(self.nodeclass_expr)
        return f"{cl_py}, args={args_py}, kwargs={kwargs_py}"
        
    def as_agent_stmt(self):
        #STUB Should add an arg 'behalf_of=_thisid'
        return BuildStmt(self.nodeclass_expr, self.args + [agent_argexpr])

    def implicit_cond_expr(self):
        return NotAlreadyBuiltExpr(self)

def positional_args(arg_exprs):
    return [a for a in arg_exprs if a.is_positional()]

def keyword_args(arg_exprs):
    '''Returns a list of tuples (name, expr).'''
    return [(a.argname, a.expr) for a in arg_exprs if not a.is_positional()]

class SeeDo(NiceRepr):

    def __init__(self, cond_exprs, action_stmts, else_see_do):
        self.cond_exprs = cond_exprs
        self.action_stmts = action_stmts
        self.else_see_do = else_see_do
        #TODO Add the implicit conditions from the action_stmts to the
        # cond_exprs

    def add_to_class(self, cl, env):
        file = Indenting(StringIO())
        fixup = Indenting(StringIO())
        ifstmt = self.make_ifstmt()
        ifstmt.add_to_env(env)
        ifstmt.gen(file, fixup, env)
        cl.add_action(DelayedGen(file, fixup))

    def make_ifstmt(self):
        return IfStmt(
            self.cond_exprs,
            coalesced_stmts(self.action_stmts),
            self.else_see_do.make_ifstmt() if self.else_see_do else None
        )

def coalesce_conditions(cond_exprs, action_stmts):
    cond_exprs = (
        as_list(cond_exprs)
        +
        as_list(s.implicit_cond_expr() for s in as_iter(action_stmts))
    )
    if not cond_exprs:
        return None
    else:
        ce = cond_exprs[0]
        for nextce in cond_exprs[1:]:
            if nextce:
                ce = ce.coalesced_with(nextce)
        return ce

def coalesced_stmts(stmts):
    stmts = as_list(stmts)
    if not stmts:
        return NullStmt()
    else:
        result = stmts[0]
        for stmt in stmts[1:]:
            result = result.coalesced_with(stmt)
        return result

class DelayedGen:

    def __init__(self, file, fixup):
        self.file = file
        self.fixup = fixup

    def gen(self, file, fixup, env):
        self.file.seek(0)
        self.fixup.seek(0)
        for line in self.file:
            print(line, file=file, end='')
        for line in self.fixup:
            print(line, file=fixup, end='')

class Postamble(EnvItem):

    def __init__(self, filename):
        self.filename = filename

    def print(self, file):
        with open(self.filename, 'r') as f:
            print(file=file)
            print('#' + (72 * '-'), file=file)
            for line in f:
                print(line, file=file, end='')

    def add_to_env(self, env):
        pass
