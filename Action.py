# Action.py -- The base Action class and some ancillary code

from abc import ABC, abstractmethod


class Action(ABC):
    '''An action to be performed on the graph.'''

    threshold = 0.0
    # weight must be >= threshold for Action.go() to be called

    weight = 0.1

    on_behalf_of = None
    # The ActiveNode, if any, that produced this action. Descendant classes
    # that implement actions for ActiveNodes should override on_behalf_of
    # in their self.__init__().

    @abstractmethod
    def go(self, g):
        '''Updates g (the host graph) and returns None.'''
        #TODO .go should return some sort of result or disposition, if only
        #to print in log files.
        pass

    __repr__ = nice_object_repr


class FuncAction(Action):
    '''Convenience class that enables treating an arbitrary function as
    an Action.'''

    def __init__(self, func, g, *args, **kwargs):
        self.func = func
        self.args = args
        self.kwargs = kwargs

    def go(self, g):
        return self.func(g, *self.args, **self.kwargs)

class Build(Action):
    '''Builds a node and links it to specified existing nodes.'''

    def __init__(self, nodeclass, link_specs, existing_nodes):
        if len(link_specs) != len(existing_nodes):
            raise(ValueError(f'number of link_specs ({link_specs}) does not equal number of existing nodes ({existing_nodes}).'))
        self.nodeclass = nodeclass
        self.link_specs = link_specs
        self.existing_nodes = existing_nodes

    def go(self, g):
        #TODO member_of? common container of all the existing_nodes?
        new_node = g.make_node(self.nodeclass)
        for link_spec, existing_node in self.link_specs, self.existing_nodes:
            g.add_edge(
                new_node,
                link_spec.new_node_port_label,
                existing_node,
                link_spec.old_node_port_label
            )
