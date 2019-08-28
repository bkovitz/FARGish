from PortGraph import PortGraph, pg, pn, Node, Tag, CouldMake, Number
from numbonodes import *
from watcher import Watcher, Response, TagWith
from util import nice_object_repr

from itertools import product


class Numble:

    def __init__(self, bricks, target):
        'bricks: a list of integers. target: an integer.'
        self.bricks = bricks
        self.target = target

    def build(self, g, container):
        '''Builds the nodes for the numble as members of the container node
        in graph g. Returns container.'''
        for brick in self.bricks:
            brick_id = g.make_node(Brick(brick))
            g.add_member_edge(container, brick_id)
            TagWith(Avail, taggee=brick_id).go(g)
        target_id = g.make_node(Target(self.target))
        g.add_member_edge(container, target_id)
        TagWith(Wanted, taggee=target_id).go(g)
        return container

    __repr__ = nice_object_repr


def prompt_for_numble():
    '''Prompts the user to enter bricks and a target at the keyboard.
    Returns a Numble object.'''
    while True:
        brick_str = input('Bricks: ')
        try:
            bricks = [int(b) for b in brick_str.split()]
            break
        except ValueError:
            print('Please enter the bricks as integers separated by spaces.')
            continue

    while True:
        target_str = input('Target: ')
        try:
            target = int(target_str)
            break
        except ValueError:
            print('Please enter one integer and press Enter.')
            continue

    return Numble(bricks, target)


#TODO rm
class BrickWatcher(Watcher):

    def look(self, hg):
        return [self.make_response(brick)
                    for brick in g.nodes_of_class(Brick)
                        if (not g.is_in_role(brick, 'operand')
                            and
                            not g.has_tag(brick, Avail))]

    def make_response(self, brick):
        return TagWith(Avail, taggee=brick)


class WantedWatcher(Watcher):

    def look(self, hg):
        avails = hg.nodes_with_tag(Avail)
        wants = hg.nodes_with_tag(Wanted)
        return [self.make_response(avail, want)
                    for avail, want in product(avails, wants)
                        if hg.have_same_value(avail, want)]

    def make_response(self, avail, want):
        return FoundWanted(avail, want)

class Avail(Tag):
    pass

class Wanted(Tag):
    pass

class FoundWanted(Response):
    def __init__(self, avail, want):
        self.avail = avail
        self.want = want

    def go(self, g):
        pass #TODO

    __repr__ = nice_object_repr


def run(numble):
    g = PortGraph()
    ws = g.make_node(Workspace)
    numble.build(g, ws)


if __name__ == '__main__':
#    numble = prompt_for_numble()
#    run(numble)
    g = PortGraph()
    ws = g.make_node(Workspace)
    Numble([1, 3, 1], 3).build(g, ws)
    pg(g)
    rs = WantedWatcher().look(g)
    #rs = BrickWatcher().look(g)
    print(rs)
    #rs[0].go(g)  # This should tag the first Brick with Avail

