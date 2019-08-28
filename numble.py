# numble.py -- The Numble class: defines a Numbo problem

from numbonodes import *
from util import nice_object_repr


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
        target_id = g.make_node(Target(self.target))
        g.add_member_edge(container, target_id)
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
