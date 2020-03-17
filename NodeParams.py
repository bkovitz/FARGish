# NodeParam.py -- Classes that describe parameters passed to a Node when
#                 constructing it

from abc import ABC, abstractmethod
from collections.abc import Iterable
from copy import copy
from itertools import chain

from exc import TooManyArgs0
from util import as_iter, as_set, as_name, empty_set, NiceRepr


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
    def as_kv(self):
        '''Return a (key, value) pair for this NodeParam so that it can
        be stored in a dict.'''
        pass

    @abstractmethod
    def as_filled_param(self, v):
        '''Return a FilledParam object that knows how to supply v as the
        value of this NodeParam.'''
        pass

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
        immediately after the Node is put into the graph. Must return the
        key from kwargs that was processed, or None.'''
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

    def as_kv(self):
        return (self.this_port_label, self)

    def as_filled_param(self, mateid):
        return FilledMate(self, mateid)

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
            return None
        for thatid in as_iter(mateid):
            g.add_edge(
                thisid,
                self.this_port_label,
                thatid,
                self.that_port_label
            )
        return self.this_port_label

    def arg_into_kwargs(self, arg, kwargs):
        add_kwarg(self.this_port_label, arg, kwargs)

    def is_exact_match(self, g, node, kwargs):
        kwarg_mates = as_set(kwargs.get(self.this_port_label, empty_set))
        node_mates = g.neighbors(node, port_label=self.this_port_label)
        return kwarg_mates == node_mates

class AttrParam(NodeParam):
    
    def __init__(self, name):
        self.name = name

    def as_kv(self):
        return (self.name, self)

    def as_filled_param(self, v):
        return FilledAttr(self, v)

    def __repr__(self):
        return f'AttrParam({repr(as_name(self.name))})'

    def on_init(self, datum, kwargs):
        try:
            v = kwargs[self.name]
        except KeyError:
            return
        setattr(datum, self.name, v)

    def on_build(self, g, thisid, kwargs):
        # We don't affect the node; the attribute it assumed to already have
        # been set in the Datum before the Node was built.
        return self.name

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
        self.d = dict(p.as_kv() for p in self.params)

    def on_init(self, datum, kwargs):
        for param in self.params:
            param.on_init(datum, kwargs)

    def on_build(self, g, thisid, kwargs):
        to_do = set(kwargs.keys())
        for param in self.params:
            did = param.on_build(g, thisid, kwargs)
            if did:
                to_do.discard(did)
        # Try viewing any kwargs not covered by NodeParam objects as port
        # labels, whose values are the node ids to link to.
        for port_label in to_do:
            g.auto_link(thisid, port_label, kwargs[port_label])

    def make_filled_params(self, g, kwargs):
        result = {}
        for k,v in kwargs.items():
            if k in self.d:
                result[k] = self.d[k].as_filled_param(v)
            elif g.is_port_label(k):
                result[k] = FilledMate2(k, v)
            else:
                result[k] = FilledAttr(k, v)
        return FilledParams(result)

    def args_into_kwargs(self, args, kwargs):
        '''Converts args into named arguments in kwargs. Returns a new
        kwargs.'''
        if kwargs is None:
            kwargs = {}
        else:
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

class FilledParam(ABC, NiceRepr):

    @abstractmethod
    def is_match(self, g, nodeid):
        pass

    @abstractmethod
    def potential_neighbors(self):
        pass

    @abstractmethod
    def apply_to_node(self, g, thisid):
        pass

class FilledMate(FilledParam):

    def __init__(self, mate_param, mateid):
        self.mate_param = mate_param  # a MateParam
        self.mateid = mateid  # the nodeid to link to

    def is_match(self, g, nodeid):
        return (
            nodeid
            in
            g.neighbors(nodeid, port_label=self.mate_param.this_port_label)
        )

    def potential_neighbors(self):
        return as_iter(self.mateid)

    def apply_to_node(self, g, thisid):
        g.add_edge(
            thisid,
            self.mate_param.this_port_label,
            self.mateid,
            self.mate_param.that_port_label
        )

class FilledMate2(FilledParam):
    '''Like FilledMate, but we don't the port label to link to, so we'll
    have to figure it out by looking at the node we're linking to.'''
    
    def __init__(self, this_port_label, mateid):
        self.this_port_label = this_port_label
        self.mateid = mateid

    def is_match(self, g, nodeid):
        return nodeid in g.neighbors(nodeid, port_label=self.this_port_label)

    def potential_neighbors(self):
        return as_iter(self.mateid)

    def apply_to_node(self, g, thisid):
        g.auto_link(thisid, self.this_port_label, self.mateid)

class FilledAttr(FilledParam):

    def __init__(self, attr_param, value):
        self.attr_param = attr_param
        self.value = value

    def is_match(self, g, nodeid):
        '''Always returns False, so that it's possible to make multiple
        instances of nodes like Numbers, Letters, etc.'''
        return False
        #return g.value_of(nodeid, self.attr_param.name) == self.value

    def potential_neighbors(self):
        return []

    def apply_to_node(self, g, nodeid):
        datum = g.datum(nodeid)
        setattr(datum, self.attr_param.name, self.value)

class FilledParams(NiceRepr):

    def __init__(self, fps):
        '''fps is a dictionary of FilledParam objects.'''
        #print('FPS', fps)
        self.fps = fps

    def is_match(self, g, nodeclass, nodeid):
        return (
            g.is_of_class(nodeid, nodeclass)
            and
            all(fp.is_match(g, nodeid) for fp in self.fps.values())
        )

    def potential_neighbors(self):
        return chain.from_iterable(
            fp.potential_neighbors() for fp in self.fps.values()
        )

    def apply_to_node(self, g, nodeid):
        '''Applies the filled parameters to nodeid: sets specified links, adds
        any attrs to datum object, and adds an .id member to the datum object,
        holding the node's nodeid.'''
        datum = g.datum(nodeid)
        datum.id = nodeid
        for fp in self.fps.values():
            fp.apply_to_node(g, nodeid)

#TODO rm
class Mate:

    def __init__(self, port_label, nodeid):
        self.port_label = port_label  # other node's port labe
        self.nodeid = nodeid          # other node

    def candidates(self, g, cl=None):
        '''Returns set of nodes that could have self.nodeid as a mate.'''
        return g.neighbors(self.nodeid, port_label=self.port_label)
