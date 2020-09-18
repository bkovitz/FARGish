# criteria.py -- Criterion classes to pass to PortGraph.look_for()

from abc import ABC, abstractmethod
from dataclasses import dataclass

from PortGraph import Node
from Action import Action
from ActiveNode import ActionNode


class Criterion(ABC):
    '''A callable object that takes two parameters: g, nodeid; and returns
    true iff nodeid meets a given criterion.'''

    @abstractmethod
    def __call__(self, g, nodeid):
        pass

class Tagged(Criterion):

    def __init__(self, tagclass):
        self.tagclass = tagclass

    def __call__(self, g, nodeid):
        return g.has_tag(nodeid, self.tagclass)

class HasValue(Criterion):

    def __init__(self, value):
        self.value = value

    def __call__(self, g, nodeid):
        return g.value_of(nodeid) == self.value

@dataclass
class OfClass(Criterion):
    nodeclass: Node

    def __call__(self, g, nodeid):
        return g.is_of_class(nodeid, self.nodeclass)

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
    tagclass: Node

    def __call__(self, g, nodeid):
        return not g.tags_of([self.first_node, nodeid], self.tagclass)

@dataclass
class NotNode(Criterion):
    '''Is the node not this_node?'''
    this_node: int

    def __call__(self, g, nodeid):
        return nodeid != self.this_node
