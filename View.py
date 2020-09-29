# View.py -- Definitions for the View node

#from PortGraph import Node, Tag, NodesWithSalience
from Node import Node
from watcher import Watcher, Response, Decision, TagWith2
from util import nice_object_repr


class View(Node):
    '''A node that "sees" all other nodes of a given type, updating
    automatically and enabling tracking of whole-view attributes such
    as the current number of nodes of the given type, maximum and
    minimum Numbers with the View, and coarse-grained representations
    such as a number line that holds all the Numbers within the View.'''

    def __init__(self, criterion):
        self.criterion = criterion

    def after_touch_update(self, touched_nodes, new_nodes):
        '''Update 'viewing' and 'view' links for newly created nodes.'''
        for viewee in self.viewing():
            if not self.criterion(self.g, viewee):
                self.g.remove_edge(self, 'viewing', viewee, 'view')
        for new_node in new_nodes:
            if self.criterion(self.g, new_node):
                self.g.add_edge(self, 'viewing', new_node, 'view')

    def viewing(self):
        '''Returns iterable of nodes being viewed by this node.'''
        return self.g.neighbors(self, port_label='viewing')


class NodeCriterion:
    '''A function that returns True iff a given node is of a given nodeclass
    and, optionally, is tagged with a given tagclass.'''

    def __init__(self, nodeclass=Node, tagclass=None):
        self.nodeclass = nodeclass
        self.tagclass = tagclass
        if self.tagclass is None:
            self._is_tagged = lambda g, node: True
        else:
            self._is_tagged = lambda g, node: g.has_tag(node, self.tagclass)

    def __call__(self, g, node):
        return g.is_of_class(node, self.nodeclass) and self._is_tagged(g, node)

    __repr__ = nice_object_repr
