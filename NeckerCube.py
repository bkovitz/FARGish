
from PortGraph import PortGraph, pg, ps
from TimeStepper import TimeStepper
from codegen import make_python, compile_fargish
from log import *
import support

prog = '''
neighbors -- neighbors
Corner
Line
'''

#make_python(prog, debug=1)  # Uncomment this to see generated code
exec(compile_fargish(prog, saveto='necker.gen.py'), globals())

class NeckerCubeGraph(TimeStepper, PortGraph):
    port_mates = port_mates

def new_graph(seed=None):
    g = PortGraph(seed=seed)
    c1, c2, c3, c4, c5, c6, c7, c8 = [g.make_node(Corner) for i in range(8)]
    l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12 = [
        g.make_node(Line) for i in range(12)
    ]
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

    return g

if __name__ == '__main__':
    g = new_graph(seed=1)
    pg(g)
