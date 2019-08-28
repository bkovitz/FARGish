# watcher.py -- Watcher and Response

from PortGraph import nice_object_repr

from abc import ABC, abstractmethod
from collections.abc import Iterable

class Watcher(ABC):

    @abstractmethod
    def look(self, hg, nodes):
        'Should return a collection of Response objects.'
        pass

    @property
    def salience(self):
        return 1.0


class Response(ABC):

    @property
    def salience(self):
        return 1.0

    @abstractmethod
    def go(self, hg):
        'Updates hg (the host graph) and returns None.'
        pass


class TagWith(Response):

    def __init__(self, tag_class, tag_args=(), edges={}, taggee=None):
        '''edges is a dict {port_label: node}'''
        #TODO 'edges' should allow caller to specify the taggee's port label,
        # too.
        self.tag_class = tag_class
        self.tag_args = tag_args
        if taggee is None:
            self.edges = edges
        else:
            self.edges = {'taggees': taggee}

    __repr__ = nice_object_repr

    def go(self, g):
        tag = g.make_node(self.tag_class(*self.tag_args))
        for port_label, node in self.edges.items():
            #TODO UGLY Find some standard way to handle passing a single
            #node or an iterable.
            if isinstance(node, Iterable):
                for node in node:
                    g.add_edge(tag, port_label, node, 'tags')
            else:
                g.add_edge(tag, port_label, node, 'tags')
