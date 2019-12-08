# numbospec.py -- Data structures that specify the Numbo model

from FARGSpec import FARGSpec, EdgeInfo
from PortGraph import Node, Tag, NodesWithSalience, pg, ps
from bases import ActiveNode, Action
from criteria import Tagged, HasValue
from exc import NumboSuccess
from util import nice_object_repr
import expr

from abc import ABC, abstractmethod

edgeinfos = [
    EdgeInfo('taggees', 'tags', clas='Tag'),
    EdgeInfo('support_from', 'support_to', clas='Support'),
    EdgeInfo('members', 'member_of', clas='Member'),
    EdgeInfo('viewing', 'view', clas='View'),
    EdgeInfo('agent_for', 'agents', clas='Agent')
]

spec = FARGSpec(edgeinfos)

# Non-tag node classes

class Workspace(Node):
    min_support_for = 0.01  # Prevents node from being removed

class Number(Node):

    def __init__(self, n):
        self.value = n

    def display_name(self, g, node):
        return self.datumstr(g, node)

    def is_attrs_match(self, other):
        try:
            return self.value == other.value
        except AttributeError:
            return False

    def expr(self, g, node):
        return expr.Number(g.value_of(node))

class Brick(Number):
    default_salience = 0.1
    min_support_for = 0.1
    #gives_reciprocal_support = False

class Target(Number):
    needs_source = True
    default_salience = 1.0
    min_support_for = 1.0

class Block(Number):
    needs_source = True

    def expr(self, g, node):
        source = g.neighbor(node, port_label='source')
        #TODO What if there's more than one source? Or none?
        return g.expr(source)


# Tag classes and ancillary functions

class Avail(Tag):
    pass

class Failed(Tag):
    pass

class WantBuiltFromBricks(Tag, ActiveNode):
    min_support_for = 1.0

    def actions(self, g, thisid):
        wantedid = g.taggee_of(thisid)
        wanted_number = g.value_of(wantedid)
        foundid = g.look_for(Tagged(Avail), HasValue(wanted_number))
        if is_built_from_bricks(g, foundid):
            responses = [HaltNumbo(NumboSuccess(g, wantedid))]
        else:
            #wanted_number = g.taggee_value(thisid)
            responses = [] #TODO
        return responses

def is_built_from_bricks(g, nodeid):
    # True iff nodeid is a Brick or all its 'source' neighbors trace all the
    # way to Bricks.
    if not g.has_node(nodeid):
        return False
    datum = g.datum(nodeid)
    if datum.needs_source:
        sources = g.neighbors(nodeid, port_label='source')
        if sources:
            return all(is_built_from_bricks(s) for s in sources)
        else:
            return False
    else:
        return True

# Actions

class HaltNumbo(Action):

    def __init__(self, done_object):
        self.done_object = done_object

    def go(self, g):
        g.set_done(self.done_object)

# Definition of a Numbo problem, a "numble"

class Numble:

    def __init__(self, bricks, target):
        'bricks: a list of integers. target: an integer.'
        self.bricks = bricks
        self.target = target

    def build(self, g, container):
        '''Builds the nodes for the numble as members of the container node
        in graph g. Returns container.'''
        target_id = g.make_node(Target(self.target), container)
        WantBuiltFromBricks.add_tag(g, target_id)
        g.graph['target'] = target_id
        for brick in self.bricks:
            brick_id = g.make_node(Brick(brick), container)
            #TagWith(Avail, taggee=brick_id).go(g)
            g.add_tag(Avail, brick_id)
        return container

    def as_dict(self):
        return { 'bricks': self.bricks, 'target': self.target }

    __repr__ = nice_object_repr


def prompt_for_numble():
    '''Prompts the user to enter bricks and a target at the keyboard.
    Returns a Numble object, or None if user just hit Enter.'''
    print()
    try:
        while True:
            brick_str = input('Bricks: ')
            if not brick_str:
                return None
            try:
                bricks = [int(b) for b in brick_str.split()]
                if not bricks:
                    continue #TODO Probably better to throw an exception
                break
            except ValueError:
                print('Please enter the bricks as integers separated by spaces.')
                continue

        while True:
            target_str = input('Target: ')
            if not target_str:
                return None
            try:
                target = int(target_str)
                break
            except ValueError:
                print('Please enter one integer and press Enter.')
                continue

        return Numble(bricks, target)
    except EOFError:
        print()
        return None
