# criteria.py -- Criterion classes to pass to ActiveGraph.look_for()

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar
from util import as_iter, as_list

from Node import Node, NRef, PortLabels
from Action import Action
from ActiveNode import ActionNode


class Criterion(ABC):
    '''A callable object that takes two parameters: g, nodeid; and returns
    true iff nodeid meets a given criterion.'''

    @abstractmethod
    def __call__(self, g, nodeid):
        pass

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

Criteria = Union[Criterion, Iterable[Criterion], None]

class Tagged(Criterion):

    def __init__(self, tagclass):
        self.tagclass = tagclass

    def __call__(self, g, nodeid):
        return g.has_tag(nodeid, self.tagclass)

class NotTagged(Criterion):

    def __init__(self, tagclass):
        self.tagclass = tagclass

    def __call__(self, g, nodeid):
        return not g.has_tag(nodeid, self.tagclass)

@dataclass
class HasValue(Criterion):
    value: Any = None

    def __call__(self, g, nodeid):
        return g.value_of(nodeid) == self.value

HasThisValue = HasValue

class HasSameValueAs(Criterion):

    def __init__(self, anchor: NRef):
        self.anchor = anchor

    def __call__(self, g, nodeid):
        #TODO Return False if either value is None?
        return g.value_of(nodeid) == g.value_of(self.anchor)

@dataclass
class OfClass(Criterion):
    nodeclass: Type[Node]

    def __call__(self, g, nodeid):
        return g.is_of_class(nodeid, self.nodeclass)

#    def __str__(self):
#        return self.nodeclass.__name__
#
#    __repr__ = __str__

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
