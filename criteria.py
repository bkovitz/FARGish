# criteria.py -- Criterion classes to pass to ActiveGraph.look_for()

from abc import ABC, abstractmethod
from dataclasses import dataclass, fields, replace
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable
from util import as_iter, as_list

from Node import Node, NodeId, NRef, MaybeNRef, PortLabel, PortLabels, MaybeCRef
from Action import Action
from ActiveNode import ActionNode


@dataclass
class Criterion(ABC):
    '''A callable object that takes two parameters: g, nodeid; and returns
    true iff nodeid meets a given criterion.'''

    @abstractmethod
    def __call__(self, g: 'G', nref: MaybeNRef):
        pass

    def replace_from_env(
        self, g: 'G', ac: 'Ac', actor: MaybeNRef, env: 'AcEnv'
    ) -> 'Criterion':
        '''Returns a new Criterion of the same class but with its argument
        names given replacement values that are defined in 'env'. If env
        provides no replacement values, might return this Criterion.'''
        changes = {}
        for field_name in (f.name for f in fields(self)):
            oldv = getattr(self, field_name)
            newv = ac.get(g, actor, env, field_name, default=oldv)
            if newv is not None and newv != oldv:
                changes[field_name] = newv
        if changes:
            return replace(self, **changes)
        else:
            return self

    # TODO rm?
    @classmethod
    def append(
        cls,
        ls: Union[List['Criterion'], 'Criterion', None],
        cs: Union[List['Criterion'], 'Criterion', None]
    ) -> Union[List['Criterion'], None]:
        if not cs:
            return ls
        elif ls is None:
            return cs
        else:
            print('APPEND1', ls)
            ls = as_list(ls)
            print('APPEND2', ls)
            for c in as_iter(cs):
                ls.append(c)
            return ls

#Criteria = Union[Criterion, Iterable[Criterion], None]
Criteria = Sequence[Criterion]

# TODO UT
@dataclass
class NodeEq(Criterion):
    node: Node

    def __call__(self, g, nref):
        node = g.datum(nref)
        return node == self.node

@dataclass
class Tagged(Criterion):
    tagclass: MaybeCRef = None

    def __call__(self, g, nodeid):
        return g.has_tag(nodeid, self.tagclass)

@dataclass
class NotTagged(Criterion):
    tagclass: MaybeCRef = None

    def __call__(self, g, nodeid):
        return not g.has_tag(nodeid, self.tagclass)

@dataclass
class HasValue(Criterion):
    value: Any = None

    def __call__(self, g, nodeid):
        return g.value_of(nodeid) == self.value

HasThisValue = HasValue

@dataclass
class HasSameValueAs(Criterion):

    def __init__(self, anchor: NRef):
        self.anchor = anchor

    def __call__(self, g, nodeid):
        #TODO Return False if either value is None?
        return g.value_of(nodeid) == g.value_of(self.anchor)

@dataclass
class OfClass(Criterion):
    nodeclass: MaybeCRef

    def __call__(self, g, nodeid):
        return g.is_of_class(nodeid, self.nodeclass)

@dataclass
class Activated(Criterion):

    def __call__(self, g, nodeid):
        #print('ACTIVATED', nodeid, g.activation(nodeid))
        return g.activation(nodeid) >= 1.0

@dataclass
class IsAction(Criterion):
    action: Action

    def __call__(self, g, nodeid):
        return (
            g.is_of_class(nodeid, ActionNode)
            and
            g.value_of(nodeid, 'action') == self.action
        )

@dataclass
class HasAttr(Criterion):
    '''Does the given node have an attr named attr_name with a value other than
    None?'''
    attr_name: str

    def __call__(self, g, nodeid):
        return g.value_of(nodeid, attr_name=self.attr_name) is not None

@dataclass
class NotTaggedTogetherWith(Criterion):
    '''Are the given node and first_node tagged with the same tag of class
    tagclass?'''
    first_node: int
    tagclass: Type[Node]

    def __call__(self, g, nodeid):
        return not g.tags_of([self.first_node, nodeid], self.tagclass)

@dataclass
class NotNode(Criterion):
    '''Is the node not this_node?'''
    this_node: NRef

    def __call__(self, g, nodeid):
        return nodeid != self.this_node

@dataclass
class PossibleMates(Criterion):
    '''Does this node have a port label that mates with port_label?'''
    port_label: PortLabels

    def __call__(self, g, nodeid):
        # HACK Need to query for the node's port_labels and ask g.port_mates.
        if self.port_label == 'operands':
            return g.is_of_class(nodeid, 'Number')
        elif self.port_label == 'source':
            return g.is_of_class(nodeid, 'Operator')

@dataclass
class NoMate(Criterion):
    '''Does this node have no neighbor at port_label?'''
    port_label: PortLabels

    def __call__(self, g, nodeid):
        return not g.neighbors(nodeid, port_label=self.port_label)

@dataclass
class And(Criterion):
    criteria: Sequence[Criterion]

    def __init__(self, *criteria: Criterion):
        self.criteria = criteria

    def __call__(self, g, nodeid):
        return all(
            c(g, nodeid) for c in self.criteria
        )

    def replace_from_env(self, g, ac, actor, env):
        return And(*(
            c.replace_from_env(g, ac, actor, env) for c in self.criteria
        ))

@dataclass
class NotTheArgsOf:
    '''Tuple condition for Cartesian product of nodes: the nodes in the tuple
    must not be the complete set of neighbors at port label 'role' relative to
    a node that meets neighbor_criterion. For example, if you want to specify
    that a tuple must not contain the operands of an existing Plus node, then
    you write NotTheArgsOf(Plus, 'operands').'''
    neighbor_criterion: Union[Criterion, Node, NRef]
    role: PortLabel

    def __call__(self, g: 'G', tup: Tuple[NodeId]) -> bool:
        nodes = set(tup)
        neighbors = g.neighbors(tup, neighbor_label=self.role)
        return not any(
            g.neighbors(n, port_label=self.role) == nodes
                for n in neighbors
        )
