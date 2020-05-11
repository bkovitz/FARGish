
from PortGraph import PortGraph, pg, ps
from TimeStepper import TimeStepper
from codegen import make_python, compile_fargish
from log import *
from util import intersection, first
import support

prog = '''
tags -- taggees
neighbors -- neighbors

Tag(taggees)
ZTag(value) : Tag
SameZ, DiffZ : Tag

Feature
Corner, Line : Feature
'''

#make_python(prog, debug=1)  # Uncomment this to see generated code
exec(compile_fargish(prog, saveto='necker.gen.py'), globals())

Feature.min_support_for = 1.0

class NeckerCubeGraph(TimeStepper, PortGraph):
    port_mates = port_mates

def z0_of(g, nodeid):
    '''Returns id of ZTag on nodeid with value 0.'''
    return next(ztagid
        for ztagid in g.neighbors(nodeid, neighbor_class=ZTag)
            if g.value_of(ztagid) == 0
    )

def z1_of(g, nodeid):
    '''Returns id of ZTag on nodeid with value 1.'''
    return next(ztagid
        for ztagid in g.neighbors(nodeid, neighbor_class=ZTag)
            if g.value_of(ztagid) == 1
    )

def line_between(g, cornerid1, cornerid2):
    '''Returns id of Line node between cornerid1 and cornerid2.'''
    return first(intersection(
        g.neighbors(cornerid1, neighbor_class=Line),
        g.neighbors(cornerid2, neighbor_class=Line)
    ))

def samez_between(g, cornerid1, cornerid2):
    '''Returns set of SameZ nodeids between cornerid1 and cornerid2. The SameZ
    nodes actually tag the ZTag nodes that tag the corners.'''
    return intersection(
        g.at_last_step(cornerid1, ZTag, SameZ),
        g.at_last_step(cornerid2, ZTag, SameZ)
    )

def diffz_between(g, cornerid1, cornerid2):
    '''Returns set of DiffZ nodeids between cornerid1 and cornerid2. The DiffZ
    nodes actually tag the ZTag nodes that tag the corners.'''
    return intersection(
        g.at_last_step(cornerid1, ZTag, DiffZ),
        g.at_last_step(cornerid2, ZTag, DiffZ)
    )

def new_graph(seed=None):
    g = NeckerCubeGraph(
        seed=seed,
        support_propagator=support.Propagator(max_total_support=70,  #300
                                              positive_feedback_rate=0.1,
                                              sigmoid_p=0.5,
                                              alpha=0.95
                                             )
    )
    c1, c2, c3, c4, c5, c6, c7, c8 = [
        g.make_node(Corner, name=f"c{i+1}") for i in range(8)
    ]
    corners = [c1, c2, c3, c4, c5, c6, c7, c8]
    l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12 = [
        g.make_node(Line, name=f"l{i+1}") for i in range(12)
    ]
    lines = [l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12]
    chains = [
        (c1, l1, c2, l7, c6, l9, c5, l5, c1),
        (c3, l4, c4, l8, c8, l12, c7, l6, c3),
        (c1, l2, c3),
        (c2, l3, c4),
        (c6, l11, c8),
        (c5, l10, c7)
    ]
    for chain in chains:
        for i in range(len(chain) - 1):
            fromid, toid = chain[i], chain[i + 1]
            g.add_edge(fromid, 'neighbors', toid, 'neighbors')

    for c in corners:
        z0 = g.make_node(ZTag, value=0, taggees=c)
        z1 = g.make_node(ZTag, value=1, taggees=c)
        g.add_mutual_opposition(z0, z1)

    level_corner_pairs = [
        (c1, c2), (c2, c6), (c6, c5), (c5, c1),
        (c3, c4), (c4, c8), (c8, c7), (c7, c3)
    ]
    updown_corner_pairs = [
        (c1, c3), (c2, c4), (c5, c7), (c6, c8)
    ]
    all_corner_pairs = level_corner_pairs + updown_corner_pairs

    for cA, cB in all_corner_pairs:
        g.add_tag(SameZ, [z0_of(g, cA), z0_of(g, cB)], mutual_support=True)
        g.add_tag(SameZ, [z1_of(g, cA), z1_of(g, cB)], mutual_support=True)
        g.add_tag(DiffZ, [z0_of(g, cA), z1_of(g, cB)], mutual_support=True)
        g.add_tag(DiffZ, [z1_of(g, cA), z0_of(g, cB)], mutual_support=True)

    for cA, cB in level_corner_pairs:
        #print('PAIR', cA, cB, line_between(g, cA, cB))
        g.add_mutual_support(line_between(g, cA, cB), samez_between(g, cA, cB))

    for cA, cB in updown_corner_pairs:
        g.add_mutual_support(line_between(g, cA, cB), diffz_between(g, cA, cB))

    return g

if __name__ == '__main__':
    g = new_graph(seed=1)
    #pg(g)
