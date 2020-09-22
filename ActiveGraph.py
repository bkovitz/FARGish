# ActiveGraph.py -- The Graph class needed for defining FARG models, and
#                   supporting classes

from abc import ABC, abstractmethod
from typing import Union, List, Set, FrozenSet, Iterable, Any, NewType, Type, \
    ClassVar
from dataclasses import dataclass, field

from NodeParams import NodeParams, FilledParams
from util import repr_str


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

    def __init__(self, *args, **kwargs):
        if len(args) == 1 and isinstance(args[0], FilledParams):
            #TODO apply FilledParams, just Attrs
            pass
        else:
            # Initialize via .node_params, but since we don't have access to
            # the graph here in __init__, we only call .node_params.on_init().
            try:
                kwargs = self.node_params.args_into_kwargs(args, kwargs)
            except TooManyArgs0 as exc:
                num_args = len(exc.args)
                raise TooManyArgs(
f'''{self.__class__.__name__}: More arguments ({len(exc.args)}) than parameters ({len(self.node_params)}): {repr(exc.args)}.'''
                )
            self.node_params.on_init(self, kwargs)
        
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

NRef = Union[NodeId, Node, None]   # A Node reference
NRefs = Union[NRef, Iterable[NRef]]

@dataclass(frozen=True)
class Hop:
    from_node: NodeId
    from_port_label: PortLabel
    to_node: NodeId
    to_port_label: PortLabel
    key: int

    def reverse(self):
        return Hop(
            self.to_node,
            self.to_port_label,
            self.from_node,
            self.from_port_label,
            self.key
        )

Hops = Union[Hop, Iterable[Hop], None]

class PortGraphPrimitives(ABC):
    nextid: NodeId = 1  # We number nodes starting from 1

    def _bump_nextid(self) -> NodeId:
        result = self.nextid
        self.nextid += 1
        return result

    @abstractmethod
    def datum(self, nodeid: NRef) -> Union[Node, None]:
        pass

    @abstractmethod
    def _add_node(self, datum: Node) -> NodeId:
        '''Should assign the node a unique nodeid, set datum.id to that id,
        set datum.g to the graph, and return the nodeid.'''
        pass

    @abstractmethod
    def _add_edge(
        self,
        node1: NodeId,
        port_label1: PortLabel,
        node2: NodeId,
        port_label2: PortLabel,
        **attr
    ) -> int:
        pass

    @abstractmethod
    def _remove_node(self, nodeid: NodeId):
        pass

    @abstractmethod
    def _remove_edge(
        self,
        node1: NodeId,
        port_label1: PortLabel,
        node2: NodeId,
        port_label2: PortLabel
    ):
        pass

    @abstractmethod
    def _neighbors(self, nodeid: NodeId) -> Iterable[NodeId]:
        pass

    @abstractmethod
    def remove_hop(self, hop: Hops):
        pass

    def find_hop(
        self,
        from_node: NodeId,
        from_port_label: PortLabel,
        to_node: NodeId,
        to_port_label: PortLabel
    ) -> Union[Hop, None]:
        '''Returns the Hop if it exists, else None.'''
        try:
            return next(hop for hop in self.hops_to_neighbor(from_node, to_node)
                                if hop.from_port_label == from_port_label
                                    and
                                    hop.to_port_label == to_port_label)
        except StopIteration:
            return None

    @abstractmethod
    def hops_from_node(self, nodeid: MaybeNodeId) -> FrozenSet[Hop]:
        pass

    @abstractmethod
    def hops_from_port(
        self, nodeid: MaybeNodeId, port_label: PortLabel
    ) -> FrozenSet[Hop]:
        pass

    @abstractmethod
    def hops_to_neighbor(
        self, nodeid: NodeId, neighborid: NodeId
    ) -> FrozenSet[Hop]:
        pass

    @abstractmethod
    def _port_labels(self, nodeid: MaybeNodeId) -> PortLabels:
        '''Should return only port labels on nodeid that actually have
        Hops connected to them.'''
        pass

    @abstractmethod
    def _nodeids(self) -> Iterable[NodeId]:
        '''Should return all NodeIds.'''
        pass

    @abstractmethod
    def _nodes(self) -> Iterable[Node]:
        '''Should return all Nodes (datum, not nodeid).'''
        pass

    @abstractmethod
    def num_nodes(self) -> int:
        pass

    @abstractmethod
    def num_edges(self) -> int:
        pass

    def __len__(self):
        return self.num_nodes()

class ActiveGraphPrimitives(PortGraphPrimitives):
    @abstractmethod
    def add_node(
        self,
        node: Union[Type[Node], Node],
        *args,
        **kwargs
    ) -> Node:
        '''If node is a Node object, creates the node and fills in its .id.
        If node is a Node class, creates the Node object from args and
        kwargs. Either way, returns the Node object.'''
        pass

    @abstractmethod
    def add_edge(
        self,
        node1: NRefs,
        port_label1: PortLabels,
        node2: NRefs,
        port_label2: PortLabels,
        **attr
    ):
        pass

    @abstractmethod
    def has_edge(self, u, v, w=None, y=None) -> bool:
        pass

    @abstractmethod
    def remove_edge(
        self,
        node1: NRefs,
        port_label1: PortLabels,
        node2: NRefs,
        port_label2: PortLabels,
    ):
        pass

    @abstractmethod
    def neighbors(
        self,
        node: NRefs,
        port_label: PortLabels,
        neighbor_class: Union[Type[Node], NodeId, None],
        neighbor_label: PortLabels
    ) -> Set[NodeId]:
        pass

    def nodes(self) -> Iterable[Node]:
        return self._nodes()

    def nodeids(self) -> Set[NodeId]:
        return set(self._nodeids())

class Building(ActiveGraphPrimitives):
    pass

class Activation(ActiveGraphPrimitives):
    pass

class Support(ActiveGraphPrimitives):
    pass

class Touches:
    pass

class Members(ActiveGraphPrimitives):
    pass

class ActiveGraph(
    Building, Activation, Support, Touches, Members, ActiveGraphPrimitives 
):
    # Overrides for ActiveGraphPrimitives

    def add_node(
        self,
        node: Union[Type[Node], Node, str],
        *args,
        **kwargs
    ) -> Node:
        # Check is_already_built
        if isinstance(node, Node):
            self._add_node(node)
            # TODO Apply link FilledParams
        else:
            # If node is a str, get_nodeclass
            assert issubclass(node, Node), f'{node} is not a subclass of Node'
            filled_params = node.node_params.make_filled_params(
                self, node.node_params.args_into_kwargs(args, kwargs)
            )
            node: Node = node()
            self._add_node(node)
            filled_params.apply_to_node(self, node.id)

            # TODO Link the FilledParams
        # add_edge_to_default_container
        # on_build
        # built_by
        # logging
        # touches
        return node

    def add_edge(
        self,
        node1: NRefs,
        port_label1: PortLabels,
        node2: NRefs,
        port_label2: PortLabels,
        **attr
    ):
        pass

    def has_edge(self, u, v, w=None, y=None) -> bool:
        pass

    def remove_edge(
        self,
        node1: NRefs,
        port_label1: PortLabels,
        node2: NRefs,
        port_label2: PortLabels,
    ):
        pass

    def neighbors(
        self,
        node: NRefs,
        port_label: PortLabels,
        neighbor_class: Union[Type[Node], NodeId, None],
        neighbor_label: PortLabels
    ) -> Set[NodeId]:
        pass

    # Additional methods (not overrides)

    def do_action(self, action: 'Action'):
        pass

if __name__ == '__main__':
    g = ActiveGraph()
    
