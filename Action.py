# Action.py -- The base Action class and some ancillary code

from abc import ABC, abstractmethod
from dataclasses import dataclass
import dataclasses
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Callable
from copy import copy

from util import nice_object_repr, as_iter, NiceRepr
from Node import NRef, MaybeNRef, NRefs, CRef, as_classname
from BuildSpec import make_buildspec
from exc import NeedArg, Fizzle, UnexpectedFizzle


class BaseAction(ABC):
    '''An action to be performed on the graph.'''

    threshold = 0.0   # activation threshold
    support_threshold = 0.0

    min_urgency = 0.0

    annotation_string = None

    actor: MaybeNRef = None
    #on_behalf_of: MaybeNRef = None

    name: Union[str, None] = None  # If str, name of this Action when printed

    def __init__(self, *args, **kwargs):
        assert dataclasses.is_dataclass(self)

    @abstractmethod
    def go(self, g: 'G', actor: NRef):
        '''Updates g (the host graph) and returns None.'''
        #TODO .go should return some sort of result or disposition, if only
        #to print in log files.
        pass

    def get_kwarg(self, name):
        try:
            return self.kwargs[name]
        except KeyError:
            raise NeedArg(self, name)  # TODO kws

    def on_fizzle(self, g: 'G', actor: NRef):
        '''Called if the Action fizzles. Default implementation: g.calm().'''
        g.calm(actor)
        
    #__repr__ = nice_object_repr

    def annotation(self):
        return self.annotation_string

@dataclass
class Action(BaseAction):

    def __init__(self, name=None, **kwargs):
        super().__init__(**kwargs)
        if name:
            self.name = name

    def param_names(self):
        return set(field.name for field in dataclasses.fields(self))

    # TODO rm once Acs are done?
    def with_overrides_from(self, g, dsource: Union[NRef, Dict[str, Any]]) \
    -> 'Action':
        if g.is_nref(dsource):
            override_d = g.get_overrides(dsource, self.param_names())
            if override_d:
                return dataclasses.replace(self, **override_d)
    #            new_action = copy(self)
    #            for param_name, value in override_d.items():
    #                setattr(new_action, param_name, value)
            else:
                #return self
                return copy(self)  # HACK Because Actions are getting shared
        else:
            assert isinstance(dsource, dict)
            result = copy(self)
            result.__dict__.update(dsource)
            return result

    def __str__(self):
        if self.name:
            return self.name
        else:
            return repr(self)

Actions = Union[Action, Iterable[Action], None]

class ResetAndKeepTrying:
    '''Mix-in for Actions that, when they're blocked, should reset their actor's
    activation and not make a Blocked tag.'''

    def action_blocked(self, g, nref: 'NRef', exc: 'Fizzle'):
        g.reset_activation(nref)
    

class FuncAction(Action):
    '''Convenience class that enables treating an arbitrary function as
    an Action.'''

    def __init__(self, func, *args, **kwargs):
        self.func = func
        self.args = args
        self.kwargs = kwargs

    def go(self, g, actor):
        return self.func(g, *self.args, **self.kwargs)

    def __repr__(self):
        return f'FuncAction({self.func}, args={self.args}, kwargs={self.kwargs})'

# TODO rm? As of 5-Sep-2020, invoked only in demo2.py.
class ActionSeq(Action):
    '''A sequence of Actions. Later Actions will be performed even if
    earlier Actions fail.'''

    def __init__(self, *actions):
        self.actions = actions

    def go(self, g, actor):
        for action in self.actions:
            action.go(g)

#TODO rm?
class ActionChain(Action):
    '''A sequence of Actions. Later Actions will only be performed if all
    earlier Actions succeeded.'''

    def __init__(self, *actions: Action):
        self.actions = actions

    def go(self, g, actor):
        pass #TODO

# TODO rm
#class Build(Action):
#    '''Builds a node according to a BuildSpec.'''
#
#    def __init__(
#        self,
#        buildspec,
#        threshold=0.0
#    ):
#        self.buildspec = buildspec
#        self.threshold = threshold
#
#    @classmethod
#    def maybe_make(
#        cls,
#        g,
#        nodeclass,
#        args=(),
#        kwargs={},
#        threshold=0.0
#    ):
#        buildspec = make_buildspec(g, nodeclass, args, kwargs)
#        if buildspec.is_already_built(g):
#            return None
#        else:
#            return Build(buildspec, threshold=threshold)
#
#    def go(self, g):
#        self.buildspec.build(g)

#def make_build(g, nodeclass, args=(), kwargs={}, threshold=0.0):
#    return Build(make_buildspec(g, nodeclass, args, kwargs), threshold)

@dataclass
class Build(Action):
    cl: CRef
    args: Tuple
    kwargs: Dict

    def __init__(self, cl: CRef, *args, **kwargs):
        self.cl = cl
        self.args = args
        self.kwargs = kwargs
        super().__init__()

    def go(self, g, actor):
        g.add_node(self.cl, *self.args, **self.kwargs)

    @classmethod
    def maybe_make(cls, g, cl: CRef, *args, **kwargs) -> Union['Build', None]:
        if not g.already_built(cl, *args, **kwargs):
            return cls(cl, *args, **kwargs)

    def __str__(self):
        clname = as_classname(self.cl)
        args = ', '.join(str(a) for a in self.args)
        kwargs = ', '.join(f'{k}={v}' for k,v in self.kwargs.items())
        if args and kwargs:
            inside_parens = f'{args}, {kwargs}'
        elif args:
            inside_parens = args
        else:
            inside_parens = kwargs
        return f'Build({clname}({inside_parens}))'

@dataclass
class BuildAgent(Action):
    behalf_of: NRef
    problem: NRef

    def go(self, g, actor):
        # HACK Should ask g for the agent class, maybe more.
        #print('BUILDAG', list(g.as_nodes(self.problem)))
        for problem in as_iter(self.problem):
            reason = g.getattr(problem, 'reason')
            #print('REASON', reason)
            # TODO assumptions about 'reason'
            agent = g.add_node(
                reason.agent_nodeclass,
                behalf_of=self.behalf_of,
                problem=problem
            )
            g.boost_activation_from_to(actor, agent)
        g.sleep(self.behalf_of)

#            if isinstance(reason, NeedArg):
#                g.add_node(
#                    'FillParamScout',
#                    behalf_of=self.behalf_of,
#                    problem=problem
#                )
#            elif isinstance(reason, NeedOperands):
#                g.add_node(
#                    'LookForOperands',
#                    behalf_of=self.behalf_of,
#                    problem=problem
#                )

@dataclass
class BoostFromTo(Action):
    to_nodes: NRefs

    def go(self, g, actor):
        for to_node in self.to_nodes:
            g.boost_activation_from_to(actor, to_node, 5.0)
        g.sleep(actor)
        
@dataclass
class Raise(Action):
    '''Raises an exception with user-supplied arguments.'''

    def __init__(self, exc_class, *args, **kwargs):
        self.exc_class = exc_class
        self.args = args
        self.kwargs = kwargs

    def go(self, g, actor):
        raise self.exc_class(*self.args, **self.kwargs)

    __repr__ = nice_object_repr

@dataclass
class Fail(Action):
    '''Calls 'fail' method on given node or nodes.'''
    node_or_nodes: NRefs

    def go(self, g, actor):
        for nodeid in as_iter(self.node_or_nodes):
            g.datum(nodeid).fail(g, nodeid)

# TODO SelfDestruct should not take an argument: it should remove 'actor'.
# TODO Name-clash with SelfDestruct in Ac.py.
@dataclass
class SelfDestruct(Action):
    node: MaybeNRef

    def go(self, g, actor):
        g.remove_node(self.node)

RemoveNode = SelfDestruct

@dataclass
class FindBasis(Action):

    def go(self, g, actor):
        if g.has_neighbor_at(actor, 'basis'):
            raise Fizzle('already has basis')
        archetypal_basis = g.neighbor(actor, 'need_basis_like')
        if not archetypal_basis:
            raise UnexpectedFizzle('no archetypal_basis')
        focal_point = g.neighbors(actor, 'focal_point')
        if not focal_point:
            #raise UnexpectedFizzle('no focal_point')
            focal_point = actor
        basis = g.look_for(g.as_node(archetypal_basis), focal_point=focal_point)
        if basis:
            g.add_edge(actor, 'basis', basis, 'basis_of')
            g.wake(actor)
        else:
            # TODO FizzleAndBlock, making an active node that tries to create
            # the missing basis.
            g.sleep(actor)
            raise Fizzle('could not find basis')

