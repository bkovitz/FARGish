# Node.py -- Base classes for all nodes in an ActiveGraph

from typing import Union, List, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar
from dataclasses import dataclass, field

from NodeParams import NodeParams, FilledParams
from util import as_iter, as_list, repr_str, omit


NodeId = NewType('NodeId', int)
MaybeNodeId = Union[NodeId, None]

PortLabel = NewType('PortLabel', str)
PortLabels = Union[PortLabel, None, Iterable[PortLabel]]


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

    node_params: ClassVar[Union[NodeParams, None]] = None
    is_tag: ClassVar[bool] = False
    is_duplicable: ClassVar[bool] = False  # May multiple instances of this
                                           # exist at the same time?
    attrs_not_to_copy: ClassVar[Set[str]] = frozenset(['id', 'g'])

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

    def regen_kwargs(self) -> Dict[str, Any]:
        return omit(self.__dict__, self.attrs_not_to_copy)
        
    def is_same_node(self, other: 'Node') -> bool:
        return self.id == other.id and self == other

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

    def dict_str(self):
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

# TODO Disallow None in NRef? Should have MaybeNRef.
NRef = Union[NodeId, Node, None]     # A Node reference
NRefs = Union[NRef, Iterable[NRef]]

CRef = Union[Type[Node], NRef, str]  # A reference to a nodeclass
CRefs = Union[CRef, Iterable[CRef]]


def as_nodeid(nref: NRef) -> Union[NodeId, None]:
    if isinstance(nref, int) or nref is None:
        return nref
    assert isinstance(nref, Node)
    return nref.id

def as_node(g: 'ActiveGraph', nref: NRef) -> Union[Node, None]:
    if isinstance(nref, Node) or nref is None:
        return nref
    assert isinstance(nref, int)
    return g.datum(nref)

def as_nodeids(nrefs: NRefs) -> Set[NodeId]:
    return set(as_nodeid(nref) for nref in as_iter(nrefs) if nref)

def as_nodes(g: 'ActiveGraph', nrefs: NRefs) -> Iterable[Node]:
    return (as_node(g, nref) for nref in as_iter(nrefs) if nref)

