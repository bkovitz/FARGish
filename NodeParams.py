# NodeParam.py -- Classes that describe parameters passed to a Node when
#                 constructing it

from abc import ABC, abstractmethod
from collections.abc import Iterable
from copy import copy

from exc import TooManyArgs0
from util import as_iter, as_set, as_name, empty_set


def add_kwarg(name, arg, kwargs):
    try:
        v = kwargs[name]
    except KeyError:
        kwargs[name] = arg
        return
    if isinstance(v, list):
        v.append(arg)
    elif isinstance(v, tuple):
        kwargs[name] = list(v) + [arg]
    else:
        kwargs[name] = [v, arg]

class NodeParam(ABC):
    '''An abstract specification of an argument to be passed to a Node's
    constructor.'''

    @abstractmethod
    def on_init(self, datum, kwargs):
        '''Take argument value from kwargs and do whatever initialization is
        appropriate to datum (modifying datum). Called from Node.__init__()
        before the Node is put into the graph.'''
        pass

    @abstractmethod
    def on_build(self, g, nodeid, kwargs):
        '''Take argument value from kwargs and do whatever initialization is
        appropriate to the node in the graph. Called from PortGraph.make_node()
        immediately after the Node is put into the graph.'''
        pass

    @abstractmethod
    def arg_into_kwargs(self, arg, kwargs):
        '''Add arg into kwargs as a named argument. Modifies kwargs.'''
        pass

    @abstractmethod
    def is_exact_match(self, g, node, kwargs):
        '''Does 'node' in graph 'g' have exact same arguments as provided
        in kwargs?'''
        pass

class MateParam(NodeParam):

    def __init__(self, this_port_label, that_port_label):
        self.this_port_label = this_port_label
        self.that_port_label = that_port_label

    def __repr__(self):
        this = as_name(self.this_port_label)
        that = as_name(self.that_port_label)
        return f'MateParam({repr(this)}, {repr(that)})'

    def on_init(self, datum, kwargs):
        pass

    def on_build(self, g, thisid, kwargs):
        try:
            mateid = kwargs[self.this_port_label]
        except KeyError:
            return
        for thatid in as_iter(mateid):
            g.add_edge(
                thisid,
                self.this_port_label,
                thatid,
                self.that_port_label
            )

    def arg_into_kwargs(self, arg, kwargs):
        add_kwarg(self.this_port_label, arg, kwargs)

    def is_exact_match(self, g, node, kwargs):
        kwarg_mates = as_set(kwargs.get(self.this_port_label, empty_set))
        node_mates = g.neighbors(node, port_label=self.this_port_label)
        return kwarg_mates == node_mates

class AttrParam(NodeParam):
    
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return f'AttrParam({repr(as_name(self.name))})'

    def on_init(self, datum, kwargs):
        try:
            v = kwargs[self.name]
        except KeyError:
            return
        setattr(datum, self.name, v)

    def on_build(self, g, thisid, kwargs):
        pass

    def arg_into_kwargs(self, arg, kwargs):
        add_kwarg(self.name, arg, kwargs)

    def is_exact_match(self, g, node, kwargs):
        return (
            g.value_of(node, attr_name=self.name)
            ==
            kwargs.get(self.name, None)
        )

class NodeParams:

    def __init__(self, *params):
        self.params = params  # Each param should be a NodeParam

    def on_init(self, datum, kwargs):
        for param in self.params:
            param.on_init(datum, kwargs)

    def on_build(self, g, thisid, kwargs):
        for param in self.params:
            param.on_build(g, thisid, kwargs)

    def args_into_kwargs(self, args, kwargs):
        '''Converts args into named arguments in kwargs. Returns a new
        kwargs.'''
        kwargs = kwargs.copy()
        if args is None:
            return kwargs
        if len(args) > len(self.params):
            raise TooManyArgs0(args)
        for arg, param in zip(args, self.params):
            param.arg_into_kwargs(arg, kwargs)
        return kwargs

    def is_exact_match(self, g, node, kwargs):
        for param in self.params:
            if not param.is_exact_match(g, node, kwargs):
                return False
        return True

    def __len__(self):
        return len(self.params)

    def __repr__(self):
        return f'{self.__class__.__name__}({", ".join(repr(p) for p in self.params)})'

#TODO rm
class Mate:

    def __init__(self, port_label, nodeid):
        self.port_label = port_label  # other node's port labe
        self.nodeid = nodeid          # other node

    def candidates(self, g, cl=None):
        '''Returns set of nodes that could have self.nodeid as a mate.'''
        return g.neighbors(self.nodeid, port_label=self.port_label)
