# watcher.py -- Watcher and Response

from util import nice_object_repr, as_iter

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

    __repr__ = nice_object_repr


class Response(ABC):

    action_threshold = 0.0
    # salience must be >= this for Response.go to be called

    salience = 0.01

    @abstractmethod
    def go(self, hg):
        'Updates hg (the host graph) and returns None.'
        pass

    def annotation(self, g):
        return self.gstr(g)

    __repr__ = nice_object_repr

    def gstr(self, g):
        '''Like __str__ but takes a g argument so the string can describe
        nodes that the Response refers to.'''
        return repr(self)  # Override to exploit g


class Decision(Response):

    action_threshold = 3.0


#TODO Make a function to do the work in this, so we can call it conveniently
#without constructing a Response.
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

    __repr__ = nice_object_repr


class TagWith2(Response):

    def __init__(self, tagclass, taggees):
        '''Builds a node of class tagclass and edges of the form
        (tag taggees) -- (taggee tags) for each taggee. taggees is either
        a single node id or an iterable of node ids.'''
        self.tagclass = tagclass
        self.taggees = taggees

    def go(self, g):
        g.add_tag(self.tagclass, self.taggees)

    def gstr(self, g):
        return 'TagWith2(%s, taggees=%s, salience=%0.3f)' % (
            self.tagclass.__name__,
            ', '.join(g.nodestr(ee) for ee in self.taggees),
            self.salience
        )
