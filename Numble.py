# Generic Numble class

from util import NiceRepr


def make_numble_class(brick, target, want, avail, allowed, rators):
    class Numble(NiceRepr):
        Brick = brick
        Target = target
        Want = want
        Avail = avail
        Allowed = allowed
        operators = rators  # List of Operator nodeclasses

        def __init__(self, bricks, target):
            self.bricks = bricks
            self.target = target

        def build(self, g, container):
            #TODO Put the nodes inside container
            target_id = g.make_node(self.Target, self.target)
            #g.add_tag(self.Want, target_id)
            #g.add_tag(self.Want, target_id, tag_port_label='target')
            g.make_node(self.Want, target_id)
            g.graph['target'] = target_id
            for brick in self.bricks:
                brick_id = g.make_node(self.Brick, brick)
                g.add_tag(self.Avail, brick_id)
            for operator in self.operators:
                operator_id = g.make_node(operator)
                g.add_tag(self.Allowed, operator_id)
            return container
            
        def as_dict(self):
            return { 'bricks': self.bricks, 'target': self.target }

    return Numble
