# Action.py -- The base Action class and some ancillary code

from abc import ABC, abstractmethod
from dataclasses import dataclass
import dataclasses
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Callable
from copy import copy

from util import nice_object_repr, as_iter, NiceRepr
from Node import MaybeNRef, NRefs, CRef
from BuildSpec import make_buildspec
from exc import NeedArg


class BaseAction(ABC):
    '''An action to be performed on the graph.'''

    threshold = 0.01   # activation threshold
    support_threshold = 0.0

    min_urgency = 0.0

    annotation_string = None

    actor: MaybeNRef = None
    #on_behalf_of: MaybeNRef = None

    def __init__(self, *args, **kwargs):
        assert dataclasses.is_dataclass(self)
        print('ACTOR', self.__class__.__name__, args, kwargs, self.actor)


    @abstractmethod
    def go(self, g: 'G'):
        '''Updates g (the host graph) and returns None.'''
        #TODO .go should return some sort of result or disposition, if only
        #to print in log files.
        pass

    def get_kwarg(self, name):
        try:
            return self.kwargs[name]
        except KeyError:
            raise NeedArg(self, name)

    #__repr__ = nice_object_repr

    def annotation(self):
        return self.annotation_string

@dataclass
class Action(BaseAction):

    def param_names(self):
        return set(field.name for field in dataclasses.fields(self))

    def with_overrides_from(self, g, nodeid):
        override_d = g.get_overrides(nodeid, self.param_names())
        if override_d:
            return dataclasses.replace(self, **override_d)
#            new_action = copy(self)
#            for param_name, value in override_d.items():
#                setattr(new_action, param_name, value)
        else:
            #return self
            return copy(self)  # HACK Because Actions are getting shared

Actions = Union[Action, Iterable[Action], None]


class FuncAction(Action):
    '''Convenience class that enables treating an arbitrary function as
    an Action.'''

    def __init__(self, func, *args, **kwargs):
        self.func = func
        self.args = args
        self.kwargs = kwargs

    def go(self, g):
        return self.func(g, *self.args, **self.kwargs)

    def __repr__(self):
        return f'FuncAction({self.func}, args={self.args}, kwargs={self.kwargs})'

# TODO rm? As of 5-Sep-2020, invoked only in demo2.py.
class ActionSeq(Action):
    '''A sequence of Actions. Later Actions will be performed even if
    earlier Actions fail.'''

    def __init__(self, *actions):
        self.actions = actions

    def go(self, g):
        for action in self.actions:
            action.go(g)

class ActionChain(Action):
    '''A sequence of Actions. Later Actions will only be performed if all
    earlier Actions succeeded.'''

    def __init__(self, *actions: Action):
        self.actions = actions

    def go(self, g):
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

    def go(self, g):
        g.add_node(self.cl, *self.args, **self.kwargs)

    @classmethod
    def maybe_make(cls, g, cl: CRef, *args, **kwargs) -> Union['NEWBuild', None]:
        if not g.already_built(cl, *args, **kwargs):
            return cls(cl, *args, **kwargs)

class Raise(Action):
    '''Raises an exception with user-supplied arguments.'''

    def __init__(self, exc_class, *args, **kwargs):
        self.exc_class = exc_class
        self.args = args
        self.kwargs = kwargs

    def go(self, g):
        raise self.exc_class(*self.args, **self.kwargs)

    __repr__ = nice_object_repr

@dataclass
class Fail(Action):
    '''Calls 'fail' method on given node or nodes.'''
    node_or_nodes: NRefs

    def go(self, g):
        for nodeid in as_iter(self.node_or_nodes):
            g.datum(nodeid).fail(g, nodeid)

@dataclass
class SelfDestruct(Action):
    node: MaybeNRef

    def go(self, g):
        g.remove_node(self.node)

RemoveNode = SelfDestruct
