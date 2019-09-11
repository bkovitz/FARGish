# numbonodes.py -- Class definitions for nodes needed by Numbo

from PortGraph import Node, Tag
from watcher import Watcher, Response, Decision
from exc import NumboSuccess
import expr
from util import as_iter

from abc import ABC, abstractmethod
from random import shuffle, choice
from itertools import permutations, product


class Workspace(Node):
    pass

class Number(Node):

    def __init__(self, n):
        self.value = n

    def is_attrs_match(self, other):
        try:
            return self.value == other.value
        except AttributeError:
            return False

    #TODO rm
    def expr_str(self, g, node):
        source = g.neighbor(node, 'source')
        if source is None:
            return str(self)
        source_datum = g.datum(source)
        return source_datum.expr_str(g, source) + ' = ' + str(self)

    def expr(self, g, node):
        return expr.Number(g.value_of(node))

class Brick(Number):
    default_salience = 1.0
    min_support_for = 1.0

class Target(Number, Watcher):
    needs_source = True
    default_salience = 1.0
    min_support_for = 2.0

    def look(self, g, node, nodes=None):
        if g.is_fully_sourced(node):
            return [HaltNumbo(NumboSuccess(g, node))]
        return []

class Block(Number):
    needs_source = True

    def expr(self, g, node):
        source = g.neighbor(node, port_label='source')
        #TODO What if there's more than one source? Or none?
        return g.expr(source)


class Avail(Tag):
    pass

class Failed(Tag):
    pass

def maybe_give_support(responses, g, from_node, to_node):
    '''Appends a GiveSupport object if from_node does not already support
    to_node.'''
    if not g.supports(from_node, to_node):
        responses.append(GiveSupport(from_node, to_node))

class Wanted(Tag, Watcher):

    def look(self, g, this_tag):
        candidates = g.nodes
        wanteds = g.neighbors(this_tag, port_label=self.tag_port_label)
        responses = []
        for candidate, wanted in product(candidates, wanteds):
            if candidate != wanted and g.have_same_value(candidate, wanted):
                if g.has_tag(candidate, Avail):
                    responses.append(ConsumeSource(candidate, wanted))
                    maybe_give_support(responses, g, this_tag, candidate)
                elif could_be_made(g, candidate):
                    maybe_give_support(responses, g, wanted, candidate)
        return responses


class Consumed(Tag):
    pass


def consume(g, avail, result):
    '''Replaces Avail tag(s) on avail node(s) with Consumed tags.
    Adds Avail tag(s) to result node(s), removing any Consumed tags.'''
    for node in as_iter(avail):
        g.remove_tag(node, Avail)
        g.add_tag(Consumed, node)
    for node in as_iter(result):
        g.remove_tag(node, Consumed)
        g.add_tag(Avail, node)


class CouldMake(Decision):
    # abstract class
    pass

def could_be_made_by(g, node):
    return (t for t in g.neighbors(node, port_label='could_make')
                if (
                    g.has_edge(t, 'result', node, 'could_make')
                    and
                    not g.has_tag(t, Failed)
                )
            )

def could_be_made(g, node):
    '''Is node tagged by an non-Failed CouldMake tag?'''
    try:
        next(could_be_made_by(g, node))
        return True
    except StopIteration:
        return False


class ConsumeSource(Decision):
    '''Consumes avail, making wanted its consumer and avail wanted's source.'''

    action_threshold = 2.0

    def __init__(self, avail, wanted):
        self.avail = avail
        self.wanted = wanted

    def go(self, g):
        consume(g, self.avail, self.wanted)
        g.add_edge(self.avail, 'consumer', self.wanted, 'source')

    def annotation(self, g):
        return (
            "Hey, look! We wanted %s and we have %s."
            %
            (g.datumstr(self.wanted), g.datumstr(self.avail))
        )

class GiveSupport(Response):

    def __init__(self, from_node, to_node):
        self.from_node = from_node
        self.to_node = to_node

    def go(self, g):
        g.add_mutual_support(self.from_node, self.to_node)

    def annotation(self, g):
        return "%s is now supporting %s and vice versa" % (
            g.datumstr(self.from_node),
            g.datumstr(self.to_node)
        )

class HaltNumbo(Response):

    def __init__(self, done_object):
        self.done_object = done_object

    def go(self, g):
        g.set_done(self.done_object)

    def annotation(self, g):
        return str(self.done_object)
