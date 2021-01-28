# NodeParam.py -- Classes that describe parameters passed to a Node when
#                 constructing it

from abc import ABC, abstractmethod
from collections.abc import Iterable
from copy import copy
from itertools import chain, filterfalse
from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Callable

from exc import TooManyArgs0
from util import as_iter, as_set, as_name, empty_set, NiceRepr, filter_none, \
    short
from log import ShowIsMatch


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

    #TODO Make this the abstractmethod and as_kv the derived method.
    def as_key(self):
        return self.as_kv()[0]

    @abstractmethod
    def as_filled_param(self, v):
        '''Return a FilledParam object that knows how to supply v as the
        value of this NodeParam.'''
        raise NotImplementedError

    @abstractmethod
    def as_default_filled_param(self):
        '''Return a FilledParam object that supplies the default value of this
        NodeParam.'''
        raise NotImplementedError

    @abstractmethod
    def on_init(self, datum, kwargs):
        '''Take argument value from kwargs and do whatever initialization is
        appropriate to datum (modifying datum). Called from Node.__init__()
        before the Node is put into the graph.'''
        raise NotImplementedError

    #TODO rm? BuildSpec and FilledParams do this now.
    @abstractmethod
    def on_build(self, g, nodeid, kwargs):
        '''Take argument value from kwargs and do whatever initialization is
        appropriate to the node in the graph. Called from PortGraph.make_node()
        immediately after the Node is put into the graph. Must return the
        key from kwargs that was processed, or None.'''
        raise NotImplementedError

    @abstractmethod
    def arg_into_kwargs(self, arg, kwargs):
        '''Add arg into kwargs as a named argument. Modifies kwargs.'''
        raise NotImplementedError

    @abstractmethod
    def is_exact_match(self, g, node, kwargs):
        '''Does 'node' in graph 'g' have exact same arguments as provided
        in kwargs?'''
        raise NotImplementedError

    @abstractmethod
    def defined_roles(self) -> 'PortLabels':
        '''What PortLabels does this NodeParam specify, from which another
        node connects to this one?'''
        raise NotImplementedError

    @abstractmethod
    def defined_port_labels(self) -> 'PortLabels':
        '''What PortLabels does this NodeParam imply to be part of the Node?'''
        raise NotImplementedError

    def display_name_pyexpr(self):
        '''Returns Python expression that evaluates to the param's value,
        suitable for generating in a nodeclass's .display_name() method.
        Returns None if this param should not be shown in .display_name().'''
        return None

    def as_node_repr_kv(self, datum):
        '''Returns a tuple (key, value), extracting 'value' from 'datum', if
        this NodeParam should appear in the string returned by Node.__repr__().
        Otherwise returns None.'''
        return None

class MateParam(NodeParam):

    def __init__(self, this_port_label, that_port_label):
        self.this_port_label = this_port_label
        self.that_port_label = that_port_label

    def as_kv(self):
        return (self.this_port_label, self)

    def as_filled_param(self, mateid):
        return FilledMate(self, mateid)

    def as_default_filled_param(self):
        return FilledMate(self, None)

    def __repr__(self):
        this = as_name(self.this_port_label)
        that = as_name(self.that_port_label)
        return f'MateParam({repr(this)}, {repr(that)})'

    def short(self):
        this = as_name(self.this_port_label)
        that = as_name(self.that_port_label)
        return f'{this} -> {that}'

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

    def defined_roles(self):
        return self.that_port_label

    def defined_port_labels(self):
        return self.this_port_label

    def is_exact_match(self, g, node, kwargs):
        kwarg_mates = as_set(kwargs.get(self.this_port_label, empty_set))
        node_mates = g.neighbors(node, port_label=self.this_port_label)
        return kwarg_mates == node_mates

class AttrParam(NodeParam):
    
    def __init__(self, name, default_value: Any=None):
        self.name = name
        self.default_value = default_value

    def as_kv(self):
        return (self.name, self)

    def as_node_repr_kv(self, datum):
        return (self.name, getattr(datum, self.name, None))

    def as_filled_param(self, v):
        return FilledAttr(self, v)

    def as_default_filled_param(self):
        return FilledAttr(self, self.default_value)

    def display_name_pyexpr(self):
        return f"self.{self.name}"

    def __repr__(self):
        if self.default_value is None:
            return f'AttrParam({repr(as_name(self.name))})'
        else:
            return f'AttrParam({repr(as_name(self.name))}, default_value={repr(self.default_value)})'

    def on_init(self, datum, kwargs):
        try:
            v = kwargs[self.name]
        except KeyError:
            return
        setattr(datum, self.name, v)

    def on_build(self, g, thisid, kwargs):
        # We don't affect the node; the attribute is assumed to already have
        # been set in the Datum before the Node was built.
        return self.name

    def arg_into_kwargs(self, arg, kwargs):
        add_kwarg(self.name, arg, kwargs)

    def defined_roles(self):
        return None

    def defined_port_labels(self):
        return None

    def is_exact_match(self, g, node, kwargs):
        return (
            g.value_of(node, attr_name=self.name)
            ==
            kwargs.get(self.name, None)
        )

@dataclass(frozen=True)
class NodeParams:

    def __init__(self, *params: NodeParam):
        object.__setattr__(self, 'params', params)
        object.__setattr__(self, 'd', dict(p.as_kv() for p in self.params))

    def names(self) -> Iterable[str]:
        '''The names of all the NodeParams.'''
        return self.d.keys()

    def on_init(self, datum: 'Node', kwargs: Dict[str, Any]):
        # Initializes datum's attributes without reference to a graph.
        # So, parameters that involving linking to other nodes are ignored.
        # Called from Node.__init__().
        for param in self.params:
            param.on_init(datum, kwargs)
        for k in set(kwargs.keys()) - self.names():
            setattr(datum, k, kwargs[k])

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
        ks = self.d.keys() | kwargs.keys()
        #for k,v in kwargs.items():
        for k in ks:
            v = kwargs.get(k, None)
            if k in self.d:
                if k in kwargs:
                    result[k] = self.d[k].as_filled_param(v)
                else:
                    result[k] = self.d[k].as_default_filled_param()
            else:  # else it's a 'custom' arg, not in the Node's definition
                if g.is_port_label(k) and g.is_nrefs(v):
                    if v:
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
            raise TooManyArgs0(self, args)
        for arg, param in zip(args, self.params):
            param.arg_into_kwargs(arg, kwargs)
        return kwargs

    def defined_roles(self) -> List['PortLabel']:
        '''Returns list of roles, i.e. port labels from which a neighboring node
        can connect to this one, that are defined in this NodeParams.'''
        return chain(as_iter(param.defined_roles()) for param in self.params)

    def defined_port_labels(self) -> Iterable['PortLabel']:
        '''Returns list of port labels defined in this NodeParams.'''
        return chain.from_iterable(
            as_iter(p.defined_port_labels()) for p in self.params
        )

    def is_exact_match(self, g, node, kwargs):
        for param in self.params:
            if not param.is_exact_match(g, node, kwargs):
                return False
        return True

#    def exclude_from_node_repr(self):
#        return set(param.as_key() for param in self.params
#                                     if isinstance(param, MateParam))

    def node_repr_kvs(self, datum):
        '''Returns a list of (name, value) for each NodeParam that should
        appear in the string returned by Node.__repr__().'''
        return filter_none(
            lambda p: p.as_node_repr_kv(datum),
            self.params
        )

    def __len__(self):
        return len(self.params)

    def __repr__(self):
        return f'{self.__class__.__name__}({", ".join(repr(p) for p in self.params)})'

class FilledParam(ABC, NiceRepr):

    @abstractmethod
    def is_match(self, g, nodeid):
        pass

    @abstractmethod
    def is_mateparam(self, g, nodeid):
        '''Should return True iff the FilledParam represents a link to
        another node, i.e. a mate.'''
        pass

    @abstractmethod
    def potential_neighbors(self):
        pass

    @abstractmethod
    def apply_to_node(self, g, thisid):
        pass

    @abstractmethod
    def specifies_attrs(self) -> bool:
        '''Does this FilledParams specify any attributes (as opposed to
        mates)?'''
        pass

    @abstractmethod
    def specifies_mates(self) -> bool:
        '''Does this FilledParams specify any mates to link to (as opposed to
        mates)?'''
        pass

#TODO UT
class FilledMate(FilledParam):

    def __init__(self, mate_param, mateid):
        self.mate_param = mate_param  # a MateParam
        self.mateid = mateid  # the nodeid or nodeids to link to

    def is_mateparam(self):
        return True

    def is_match(self, g, nodeid):
        neighbors = g.neighbors(
            nodeid, port_label=self.mate_param.this_port_label
        )
        #return all(m in neighbors for m in as_iter(self.mateid))
        return all(m in neighbors for m in g.as_nodeids(self.mateid))

    def potential_neighbors(self):
        return as_iter(self.mateid)

    def apply_to_node(self, g, thisid):
        g.add_edge(
            thisid,
            self.mate_param.this_port_label,
            self.mateid,
            self.mate_param.that_port_label
        )

    def specifies_attrs(self) -> bool:
        return False

    def specifies_mates(self):
        return True

    def __str__(self):
        return f'FilledMate({short(self.mate_param)}, {self.mateid})'

#TODO UT
class FilledMate2(FilledParam):
    '''Like FilledMate, but we don't know the port label to link to, so we'll
    have to figure it out by looking at the node we're linking to.'''
    
    def __init__(self, this_port_label, mateid):
        self.this_port_label = this_port_label
        self.mateid = mateid

    def is_mateparam(self):
        return True

    def is_match(self, g, nodeid):
        neighbors = g.neighbors(
            nodeid, port_label=self.this_port_label
        )
        #return all(m in neighbors for m in as_iter(self.mateid))
        return all(m in neighbors for m in g.as_nodeids(self.mateid))

    def potential_neighbors(self):
        return as_iter(self.mateid)

    def apply_to_node(self, g, thisid):
        g.auto_link(thisid, self.this_port_label, self.mateid)

    def specifies_attrs(self) -> bool:
        return False

    def specifies_mates(self):
        return True

class FilledAttr(FilledParam):

    def __init__(self, attr_param, value):
        self.attr_param = attr_param  # AttrParam or string
        #self.value = copy(value) if isinstance(value, object) else value
          #HACK  This hack introduced a new problem: passing a node as
          #      an argument produced a copy without the original node's
          #      id, rendering it useless.
          # A better hack would be to throw an exception if you pass a Node
          # as an argument to a FilledAttr. Nodes should never contain
          # Nodes or nodeids as attrs, since these are references that
          # can become stale if the referred-to node is ever deleted.
        self.value = value

    def is_mateparam(self):
        return False

    def is_match(self, g, node: 'NRef'):
        node = g.as_node(node)
        if node.filledattr_always_match(self.name()):  # HACK: needed because
            return True                                # AcNode fills in .action
                                                       # in .on_build().
        else:
            return g.value_of(node, self.name()) == self.value

    def potential_neighbors(self):
        return []

    def apply_to_node(self, g, nodeid):
        datum = g.datum(nodeid)
        try:
            name = self.attr_param.name
        except AttributeError:
            name = self.attr_param
        setattr(datum, name, self.value)

    def name(self) -> str:
        try:
            return self.attr_param.name
        except AttributeError:
            return self.attr_param

    def specifies_attrs(self) -> bool:
        return True

    def specifies_mates(self):
        return False

    def __str__(self):
        return f'FilledAttr({self.attr_param}={self.value})'

class FilledParams(NiceRepr):

    def __init__(self, fps: Dict[str, FilledParam]):
        #print('FPS', fps)
        self.fps: Dict[str, FilledParam] = fps

    def is_match(self, g, nodeclass, nodeid):
        #print('FPS', nodeid, nodeclass, g.is_of_class(nodeid, nodeclass))
        #for fp in self.fps.values():  #DEBUG
        #    print(f"  {fp} {fp.is_match(g, nodeid)}") #DEBUG
        result = (
            g.is_of_class(nodeid, nodeclass)
            and
            all(fp.is_match(g, nodeid) for fp in self.fps.values())
        )
        if ShowIsMatch.is_logging():
            ioc = g.is_of_class(nodeid, nodeclass)
            fps = all(fp.is_match(g, nodeid) for fp in self.fps.values())
            for fp in self.fps.values():
                print(fp.is_match(g, nodeid), fp)
            print(f'IS_MATCH nodeclass={nodeclass} {self} nodeid={nodeid} ioc={ioc} fps={fps} result={result}')
        return result

    # TODO Rename this: very confusing!  potential_mates()?
    def potential_neighbors(self):
        result = list(chain.from_iterable(
            fp.potential_neighbors() for fp in self.fps.values()
        ))
        #print('FPS_POT', result)
        return result

    def remove_ignored_for_dupcheck(self, nodeclass: Type['Node']):
        '''Remove any parameters that the nodeclass says should not count
        for determining whether an instance of the nodeclass has already
        been built.'''
        ignored_names = list(filter(
            nodeclass.ignored_for_dupcheck, self.fps.keys()
        ))
        for ignored_name in ignored_names:
            del self.fps[ignored_name]

    def apply_to_node(self, g, nodeid):
        '''Applies the filled parameters to nodeid: sets specified links, adds
        any attrs to datum object, and adds an .id member to the datum object,
        holding the node's nodeid.'''
        datum = g.datum(nodeid)
        datum.id = nodeid
        datum.filled_params = self
        for fp in self.fps.values():
            fp.apply_to_node(g, nodeid)

    def apply_to_node_except_mateparams(self, g, nodeid):
        '''Applies the filled parameters to nodeid, except for MateParams.
        This enables copying just params that exist inside the node.'''
        newfps = self.__class__(dict(
            (name, fp)
                for name, fp in self.fps.items()
                    if not fp.is_mateparam()
        ))
        newfps.apply_to_node(g, nodeid)

    def specifies_attrs(self) -> bool:
        '''Does this FilledParams specify any attributes (as opposed to
        mates)?'''
        return any(fp.specifies_attrs() for fp in self.fps.values())

    def specifies_mates(self) -> bool:
        '''Does this FilledParams specify any mates to link to (as opposed to
        attrs)?'''
        return any(fp.specifies_mates() for fp in self.fps.values())

    def __str__(self):
        params = ', '.join(f'{name}={val}' for name, val in self.fps.items())
        return f'FilledParams({params})'

#TODO rm
class Mate:

    def __init__(self, port_label, nodeid):
        self.port_label = port_label  # other node's port labe
        self.nodeid = nodeid          # other node

    def candidates(self, g, cl=None):
        '''Returns set of nodes that could have self.nodeid as a mate.'''
        return g.neighbors(self.nodeid, port_label=self.port_label)
