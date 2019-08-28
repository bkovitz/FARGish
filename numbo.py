from PortGraph import PortGraph, pg, pn, Node, Tag, CouldMake
from numbonodes import *
from watcher import Watcher, Response, TagWith
from exc import FargDone, NumboSuccess
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
                        if (not g.is_in_role(brick, 'source')
                            and
                            not g.has_tag(brick, Avail))]

    def make_response(self, brick):
        return TagWith(Avail, taggee=brick)


class WantedWatcher(Watcher):

    def look(self, hg):
        avails = hg.nodes_with_tag(Avail)
        wanteds = hg.nodes_with_tag(Wanted)
        return [self.make_response(avail, wanted)
                    for avail, wanted in product(avails, wanteds)
                        if hg.have_same_value(avail, wanted)]

    def make_response(self, avail, wanted):
        return FoundWanted(avail, wanted)

class Avail(Tag):
    pass

class Wanted(Tag):
    pass

class FoundWanted(Response):
    def __init__(self, avail, wanted):
        self.avail = avail
        self.wanted = wanted

    def go(self, g):
        g.add_edge(self.avail, 'consumer', self.wanted, 'source')
        g.remove_tag(self.avail, Avail)
        #TODO Declare victory if wanted is Target
        if g.is_of_class(self.wanted, Target):
            raise NumboSuccess(g, self.wanted)
        #TODO Otherwise tag wanted as Avail

    __repr__ = nice_object_repr


class NumboGraph(PortGraph):

    default_graph_attrs = dict(t=0, watchers=WantedWatcher(), done=False)

    def __init__(self, **kwargs):
        kws = self.default_graph_attrs.copy()
        kws.update(kwargs)
        super().__init__(**kws)
        ws = self.make_node(Workspace)
        self.graph['ws'] = ws
        if 'numble' in self.graph:
            self.graph['numble'].build(self, ws)
    
    def ws(self):
        return self.graph['ws']
        
    def watchers(self):
        #TODO It would likely be a better model if the Watchers were
        #themselves Nodes in the graph.
        return self.graph['watchers']

    def do_timestep(self):
        #TODO This is very crude: it executes all Responses from all Watchers.
        #We should randomly choose just one, based on salience.
        self.graph['t'] += 1
        try:
            for watcher in self.graph['watchers']:
                for response in watcher.look(g):
                    response.go(g)
        except FargDone as exc:
            print('\n' + exc.done_msg())
            self.graph['done'] = True
            
    def expr_by_sources(self, target):
        '''Returns a string representing the expression whose ultimate
        'consumer' is target.'''
        return self.datum(target).expr_str(self, target)

        
def run(numble):
    g = NumboGraph()
    ws = g.make_node(Workspace)
    numble.build(g, ws)

if __name__ == '__main__':
#    numble = prompt_for_numble()
#    run(numble)

#    g = PortGraph()
#    ws = g.make_node(Workspace)
#    Numble([1, 3, 1], 3).build(g, ws)
#    pg(g)
#    rs = WantedWatcher().look(g)
#    print(rs)
#    #rs[0].go(g)  # This should tag the first Brick with Avail

    g = NumboGraph(watchers=[WantedWatcher()], numble=Numble([1, 3, 1], 3))
    pg(g)
    g.do_timestep()
    print()
    pg(g)
