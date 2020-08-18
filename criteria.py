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
