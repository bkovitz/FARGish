from numble import Numble, prompt_for_numble
from PortGraph import PortGraph, pg, pn, Node, Tag, CouldMake, Number
from numbonodes import *
from watcher import Watcher, TagWith


class BrickWatcher(Watcher):

    def look(self, hg):
        return [self.make_response(brick)
                    for brick in g.nodes_of_class(Brick)
                        if (not g.is_in_role(brick, 'operand')
                            and
                            not g.has_tag(brick, Avail))]

    def make_response(self, brick):
        return TagWith(Avail, taggee=brick)


class Avail(Tag):
    pass

def run(numble):
    g = PortGraph()
    ws = g.make_node(Workspace)
    numble.build(g, ws)


if __name__ == '__main__':
#    numble = prompt_for_numble()
#    run(numble)
    g = PortGraph()
    ws = g.make_node(Workspace)
    Numble([1, 1, 1], 3).build(g, ws)
    pg(g)
    rs = BrickWatcher().look(g)
    print(rs)
    rs[0].go(g)  # This should tag the first Brick with Avail

