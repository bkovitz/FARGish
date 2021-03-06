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
            target_id = g.add_node(
                self.Target, self.target, member_of=container
            )
            #g.add_tag(self.Want, target_id)
            #g.add_tag(self.Want, target_id, tag_port_label='target')
            g.add_node(self.Want, target_id)
            #g.graph['target'] = target_id
            for brick in self.bricks:
                brick_id = g.add_node(self.Brick, brick, member_of=container)
                g.add_tag(self.Avail, brick_id)
            for operator in self.operators:
                operator_id = g.add_node(operator, member_of=container)
                g.add_tag(self.Allowed, operator_id)
            return container
            
        def as_dict(self):
            return { 'bricks': self.bricks, 'target': self.target }

    return Numble

def prompt_for_numble(Numble):
    '''Prompts the user to enter bricks and a target at the keyboard.
    Returns a Numble object, or None if user just hit Enter. Numble must
    be a class generated by make_numble_class().'''
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

