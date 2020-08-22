# criteria.py -- Criterion classes to pass to PortGraph.look_for()

from dataclasses import dataclass
from PortGraph import Node


class Tagged:

    def __init__(self, tagclass):
        self.tagclass = tagclass

    def __call__(self, g, nodeid):
        return g.has_tag(nodeid, self.tagclass)

class HasValue:

    def __init__(self, value):
        self.value = value

    def __call__(self, g, nodeid):
        return g.value_of(nodeid) == self.value

@dataclass
class OfClass:
    nodeclass: Node

    def __call__(self, g, nodeid):
        return g.is_of_class(nodeid, self.nodeclass)

@dataclass
class HasAttr:
    '''Does the given node have an attr named attr_name with a value other than
    None?'''
    attr_name: str

    def __call__(self, g, nodeid):
        return g.value_of(nodeid, attr_name=self.attr_name) is not None

@dataclass
class NotTaggedTogetherWith:
    '''Are the given node and first_node tagged with the same tag of class
    tagclass?'''
    first_node: int
    tagclass: Node

    def __call__(self, g, nodeid):
        return not g.tags_of([self.first_node, nodeid], self.tagclass)

@dataclass
class NotNode:
    '''Is the node not this_node?'''
    this_node: int

    def __call__(self, g, nodeid):
        return nodeid != self.this_node
