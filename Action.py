# Action.py -- The base Action class and some ancillary code

from abc import ABC, abstractmethod
from dataclasses import dataclass
import dataclasses

from util import nice_object_repr, as_iter
from BuildSpec import make_buildspec
from exc import NeedArg


@dataclass
class Action(ABC):
    '''An action to be performed on the graph.'''

    threshold = 0.0
    # .weight() must be >= threshold for Action.go() to be called

    # TODO rname 'min_urgency'
    min_weight = 0.1

    on_behalf_of = None
    # The ActiveNode, if any, that produced this action. Descendant classes
    # that implement actions for ActiveNodes should override on_behalf_of
    # in their self.__init__().
    actor = None

    annotation_string = None

    @abstractmethod
    def go(self, g):
        '''Updates g (the host graph) and returns None.'''
        #TODO .go should return some sort of result or disposition, if only
        #to print in log files.
        pass

    # TODO rename 'urgency'
    def weight(self, g):
        return max(
            g.support_for(self.actor),
            g.activation(self.actor),
            #g.salience(self.actor),  # HACK TODO rm
            self.min_weight
        )

    def get_kwarg(self, name):
        try:
            return self.kwargs[name]
        except KeyError:
            raise NeedArg(self, name)

    __repr__ = nice_object_repr

    def param_names(self):
        return set(field.name for field in dataclasses.fields(self))

    def with_overrides_from(self, g, nodeid):
        override_d = g.get_overrides(nodeid, self.param_names())
        if override_d:
            return dataclasses.replace(self, **override_d)
        else:
            return self
        

    def annotation(self):
        return self.annotation_string


class FuncAction(Action):
    '''Convenience class that enables treating an arbitrary function as
    an Action.'''

    def __init__(self, func, *args, **kwargs):
        self.func = func
        self.args = args
        self.kwargs = kwargs

    def go(self, g):
        return self.func(g, *self.args, **self.kwargs)

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

class Build(Action):
    '''Builds a node according to a BuildSpec.'''

    def __init__(
        self,
        buildspec,
        threshold=0.0
    ):
        self.buildspec = buildspec
        self.threshold = threshold

    @classmethod
    def maybe_make(
        cls,
        g,
        nodeclass,
        args=(),
        kwargs={},
        threshold=0.0
    ):
        buildspec = make_buildspec(g, nodeclass, args, kwargs)
        if buildspec.is_already_built(g):
            return None
        else:
            return Build(buildspec, threshold=threshold)

    def go(self, g):
        self.buildspec.build(g)

def make_build(g, nodeclass, args=(), kwargs={}, threshold=0.0):
    return Build(make_buildspec(g, nodeclass, args, kwargs), threshold)

class Raise(Action):
    '''Raises an exception with user-supplied arguments.'''

    def __init__(self, exc_class, *args, **kwargs):
        self.exc_class = exc_class
        self.args = args
        self.kwargs = kwargs

    def go(self, g):
        raise self.exc_class(*self.args, **self.kwargs)

class Fail(Action):
    '''Calls 'fail' method on given node or nodes.'''

    def __init__(self, node_or_nodes):
        self.node_or_nodes = node_or_nodes

    def go(self, g):
        for nodeid in as_iter(self.node_or_nodes):
            g.datum(nodeid).fail(g, nodeid)

class SelfDestruct(Action):

    def __init__(self, nodeid):
        self.nodeid = nodeid

    def go(self, g):
        g.remove_node(self.nodeid)

RemoveNode = SelfDestruct
