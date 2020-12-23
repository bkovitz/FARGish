# Node.py -- Base classes for all nodes in an ActiveGraph

from typing import Union, List, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar
from dataclasses import dataclass, field
from copy import deepcopy
from inspect import isclass

from NodeParams import NodeParams, FilledParams
from util import as_iter, as_list, repr_str, omit, loose_dict_eq


NodeId = NewType('NodeId', int)
MaybeNodeId = Union[NodeId, None]

PortLabel = NewType('PortLabel', str)
PortLabels = Union[PortLabel, None, Iterable[PortLabel]]

def is_nodeid(x):
    return isinstance(x, int)


@dataclass
class Node:
    '''.id and .g should be filled in by ActiveGraph._add_node() as soon as
    the actual node is created in the graph. We allow them to be uninitialized
    so you can create a Node object before passing it to _add_node(). This is
    also useful in unit tests.

    Two nodes are 'equal' (as concerns __eq__) if they hold the same attributes,
    even if they have different NodeIds and come from different graphs.'''

    id: NodeId = field(init=False, compare=False)
    g: 'ActiveGraph' = field(init=False, compare=False)

    node_params: ClassVar[Union[NodeParams, None]] = NodeParams()
    is_tag: ClassVar[bool] = False
    is_duplicable: ClassVar[bool] = False  # May multiple instances of this
                                           # exist at the same time?
    min_support_for: float = 0.0
    initial_support_for: float = 1.0
    min_activation: float = 0.0
    initial_activation: float = 1.0

    # Do we need to call this node's .update() method?
    needs_update: bool = False

    attrs_not_to_copy: ClassVar[Set[str]] = frozenset([
        'id', 'g', 'filled_params', 'tob', 'support_for'
    ])

    def __init__(self, *args, **kwargs):
        if len(args) == 1 and isinstance(args[0], FilledParams):
            #TODO apply FilledParams, just Attrs
            pass
        else:
            # Initialize via .node_params, but since we don't have access to
            # the graph here in __init__, we only call .node_params.on_init().
            # TODO Should we raise an error if kwargs specified a node to link
            # to?
            try:
                kwargs = self.node_params.args_into_kwargs(args, kwargs)
            except TooManyArgs0 as exc:
                num_args = len(exc.args)
                raise TooManyArgs(
f'''{self.__class__.__name__}: More arguments ({len(exc.args)}) than parameters ({len(self.node_params)}): {repr(exc.args)}.'''
                )
            self.node_params.on_init(self, kwargs)
        
    @classmethod
    def make_filled_params(cls, g, *args, **kwargs) -> FilledParams:
        return cls.node_params.make_filled_params(
            g, cls.node_params.args_into_kwargs(args, kwargs)
        )

    @classmethod
    def defined_roles(cls) -> List[PortLabel]:
        '''Returns list of roles, i.e. port labels from which a neighboring
        node connects to a node of this class, that are defined in this
        class's node_params. The same role can appear more than once in
        the list.'''
        return cls.node_params.defined_roles()
        
    def on_build(self):
        '''Called just after node is built. Enables the node to do any
        needed set-up. The default implementation does nothing.'''
        pass

    def on_touch(self):
        '''Called at end of timestep when node got touched (something happened
        to at least one of its neighbors). The default implementation does
        nothing.'''
        pass

    def regen_kwargs(self) -> Dict[str, Any]:
        '''From the kwargs dict returned by this function, you can reconstruct
        the node, excluding its links.'''
        return omit(self.__dict__, self.attrs_not_to_copy)
        # TODO deepcopy the Action objects
#        result = {}
#        for param_name, v in self.__dict__.items():
#            if param_name in self.attrs_not_to_copy:
#                continue
#            if isinstance(v, object):
#                if isinstance(v, Node):
#                    v = v.id  # HACK because can't deepcopy a Node
#                else:
#                    print('DEEP', v)
#                    v = deepcopy(v)
#            result[param_name] = v
#        return result
        
    def is_same_node(self, other: 'Node') -> bool:
        return self.id == other.id and self == other

    def __eq__(self, other):
        if self.__class__ != other.__class__:
            return False
        return loose_dict_eq(self.regen_kwargs(), other.regen_kwargs())

    def __repr__(self):
        if self.name:
            return self.name
        elif self.node_params:
            return repr_str(
                self.__class__.__name__,
                self.node_params.node_repr_kvs(self)
            )
        else:
            return self.__class__.__name__

    def __getattr__(self, name):
        '''All attrs default to None, to make them easy to override in
        subclasses.'''
        return None

    def __copy__(self):
        return self.__class__(**self.regen_kwargs())

    def nodestr(self):
        return f'{self.id:4d}: {self.display_name()}'

    def display_name(self):
        return repr(self)

    def statestr(self) -> str:
        state = self.state
        if state:
            return str(state)
        else:
            return ''

    def dict_str(self) -> str:
        '''String that shows the entire contents of this Node's __dict__,
        without any processing. Useful in debugging when you need to see
        exact representations.'''
        attr_strings = [
            f'{chr(10)}  {k}={v}' for k,v in sorted(self.__dict__.items())
        ]
        if not attr_strings:
            between_parens = ''
        else:
            between_parens = ','.join(attr_strings) + '\n'
        return f"{self.__class__.__name__}({between_parens})"

    # Methods that edit the graph

    def inhibit_all_next(self):
        for n in self.g.walk(self, 'next', include_start=False):
            self.g.set_activation_from_to(self, n, -2.0)

    def transient_inhibit_all_next(self):
        for n in self.g.walk(self, 'next', include_start=False):
            self.g.transient_inhibit(self, n)

# TODO Disallow None in NRef? Should have MaybeNRef.
NRef = Union[NodeId, Node, None]     # A Node reference
MaybeNRef = Union[NRef, None]
NRefs = Union[NRef, Iterable[NRef]]

def as_nodeid(nref: NRef) -> Union[NodeId, None]:
    if is_nodeid(nref) or nref is None:
        return nref
    assert isinstance(nref, Node), f'nref is not a Node: {repr(nref)}'
    return nref.id

def as_node(g: 'ActiveGraph', nref: NRef) -> Union[Node, None]:
    '''If nref is Node, we just return it, without checking if the node still
    exists.'''
    if isinstance(nref, Node) or nref is None:
        return nref
    assert isinstance(nref, int), f'{nref} is neither Node, int, nor None.'
    return g.datum(nref)

def as_nodeids(nrefs: NRefs) -> Set[NodeId]:
    return set(as_nodeid(nref) for nref in as_iter(nrefs) if nref)

def as_nodes(g: 'ActiveGraph', nrefs: NRefs) -> Iterable[Node]:
    '''Returns a generator of Node objects.'''
    return (as_node(g, nref) for nref in as_iter(nrefs) if nref)


CRef = Union[Type[Node], NRef, str]  # A reference to a nodeclass
CRefs = Union[CRef, Iterable[CRef]]
MaybeCRef = Union[CRef, None]

def as_classname(cref: Union[CRef, None]) -> str:
    if isclass(cref):
        return cref.__name__
    elif isinstance(cref, Node):
        return cref.display_name()
    else:
        return str(cref)

def is_abstract_cref(x: Any) -> bool:
    '''Is x a nodeclass, specified as either a string or a Node class object,
    but not as a node reference?'''
    if isinstance(x, str):
        return True
    try:
        return issubclass(x, Node)
    except TypeError:
        return False
