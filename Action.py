# Action.py -- The base Action class and some ancillary code

from abc import ABC, abstractmethod

from util import nice_object_repr, as_iter


class Action(ABC):
    '''An action to be performed on the graph.'''

    threshold = 0.0
    # weight must be >= threshold for Action.go() to be called

    weight = 0.1

    on_behalf_of = None
    # The ActiveNode, if any, that produced this action. Descendant classes
    # that implement actions for ActiveNodes should override on_behalf_of
    # in their self.__init__().

    annotation_string = None

    @abstractmethod
    def go(self, g):
        '''Updates g (the host graph) and returns None.'''
        #TODO .go should return some sort of result or disposition, if only
        #to print in log files.
        pass

    __repr__ = nice_object_repr

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

class ActionSeq(Action):
    '''A sequence of Actions. Later Actions will be performed even if
    earlier Actions fail.'''

    def __init__(self, *actions):
        self.actions = actions

    def go(self, g):
        for action in self.actions:
            action.go(g)

class Build(Action):
    '''Builds a node and links it to specified existing nodes.'''

    def __init__(self, nodeclass, link_specs, existing_nodes, kwargs=None):
        if len(link_specs) != len(existing_nodes):
            raise(ValueError(f'number of link_specs ({link_specs}) does not equal number of existing nodes ({existing_nodes}).'))
        self.nodeclass = nodeclass
        self.link_specs = link_specs
        self.existing_nodes = existing_nodes
        self.kwargs = kwargs

    def go(self, g):
        #TODO member_of? common container of all the existing_nodes?
        if self.kwargs:
            datum = self.nodeclass(**self.kwargs)
        else:
            datum = self.nodeclass()
        new_node = g.make_node(datum)
        for link_spec, existing_node in \
                zip(self.link_specs, self.existing_nodes):
            g.add_edge(
                new_node,
                link_spec.new_node_port_label,
                existing_node,
                link_spec.old_node_port_label
            )

class Build2(Action):
    '''Simpler, better version of Build?'''

    def __init__(
        self,
        nodeclass,
        args=(),
        kwargs={},
        weight=1.0,
        threshold=0.0
    ):
        self.nodeclass = nodeclass
        self.args = args
        self.kwargs = kwargs
        self.weight = weight
        self.threshold = threshold

    @classmethod
    def maybe_make(
        cls,
        g,
        nodeclass,
        args=(),
        kwargs={},
        weight=1.0,
        threshold=0.0,
        potential_neighbors=None
    ):
        if not g.already_built(
            nodeclass,
            args=args,
            kwargs=kwargs,
            potential_neighbors=potential_neighbors
        ):
            return Build2(nodeclass, args=args, kwargs=kwargs, weight=weight,
                threshold=threshold)

    def go(self, g):
        datum = self.nodeclass(*self.args, **self.kwargs)
        new_node = g.make_node(datum)
        #print('BUILD2', new_node, datum)

class Raise(Action):
    '''Raises an exception with user-supplied arguments.'''

    weight = 1.0

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
