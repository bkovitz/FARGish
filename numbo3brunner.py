# numbo3brunner.py -- Runner for numbo3b.py
#
# TODO Hopefully this can be replaced by a simple parameterization of
# ModelWrapper.

from numbo3b import *
from PortGraph import PortGraph, pg
from ExprAsEquation import ExprAsEquation
from TimeStepper import TimeStepper


# TODO Make this import Target, Want, Brick, etc. from module supplied by
# argument.
# TODO Allow caller to specified which Operators are Allowed.
class Numble:
    '''Definition of a Numbo problem, a "numble".'''

    def __init__(self, bricks, target):
        'bricks: a list of integers. target: an integer.'
        self.bricks = bricks
        self.target = target

    def build(self, g, container):
        '''Builds the nodes for the numble as members of the container node
        in graph g. Returns container.'''
        target_id = g.make_node(Target(self.target), container)
        Want.add_tag(g, target_id)
        g.graph['target'] = target_id
        for brick in self.bricks:
            brick_id = g.make_node(Brick(brick), container)
            #TagWith(Avail, taggee=brick_id).go(g)
            g.add_tag(Avail, brick_id)
        plusid = g.make_node(Plus, container)
        timesid = g.make_node(Times, container)
        Allowed.add_tag(g, plusid)
        Allowed.add_tag(g, timesid)
        return container

class Numbo3Graph(PortGraph, TimeStepper, ExprAsEquation):
    default_graph_attrs = dict(
        t=0,
        done=False,
        num_timesteps=40,
        seed=None,
        running=False
    )

    def __init__(self, **kwargs):
        kws = self.default_graph_attrs.copy()
        kws.update(kwargs)
        if kws.get('num_timesteps', None) is None:
            kws['num_timesteps'] = self.default_graph_attrs['num_timesteps']
        super().__init__(**kws)
        self.consecutive_timesteps_with_no_response = 0
        ws = self.make_node(Workspace)
        self.graph['ws'] = ws
        if 'numble' in self.graph:
            self.graph['numble'].build(self, ws)

g = None

def run(seed=None):
    global g
    g = Numbo3Graph(seed=seed, numble=Numble([4, 5, 6], 15))
    print('SEED', g.graph['seed'])
    #pg(g)
    g.do_timestep(num=70)
    #ConsumeOperands.fail(g, 23)
    #g.do_timestep()
    #g.do_timestep()
    #g.do_timestep()
    # Succeeds at last timestep with above seed and numble.

run(seed=8316664589534836549)
