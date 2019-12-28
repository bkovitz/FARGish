# numbo3.py

from numble import Numble, prompt_for_numble
from PortGraph import PortGraph, NodeAndValue, pg, pn, ps, Node, Tag, \
    CouldMake, ValueOf
from numbospec import *
from bases import CoarseView, AllNodesOfClass, AllNodesTagged
from ExprAsEquation import ExprAsEquation
from TimeStepper import TimeStepper
from log import ShowActiveNodes, ShowActionList, ShowActionsChosen
from exc import *
import support
from util import nice_object_repr, reseed, sample_without_replacement

class Numbo3Graph(TimeStepper, ExprAsEquation, PortGraph):

    default_graph_attrs = dict(
        t=0,
        done=False,
        num_timesteps=40,
        seed=None,
        running=False,
        support_propagator=support.Propagator(max_total_support=70,  #300
                                              positive_feedback_rate=0.1,
                                              sigmoid_p=0.5,
                                              alpha=0.95
                                             )
    )

    def __init__(self, **kwargs):
        super().__init__()
        kws = self.default_graph_attrs.copy()
        kws.update(kwargs)
        if kws.get('num_timesteps', None) is None:
            kws['num_timesteps'] = self.default_graph_attrs['num_timesteps']
        kws['seed'] = reseed(kws.get('seed', None))
        super().__init__(**kws)
        self.consecutive_timesteps_with_no_response = 0
        ws = self.make_node(Workspace)
        self.graph['ws'] = ws
        if 'numble' in self.graph:
            self.graph['numble'].build(self, ws)

    def ws(self):
        '''Returns nodeid of Workspace.'''
        return self.graph['ws']


def new_graph(numble, seed=None):
    g = Numbo3Graph(numble=numble, seed=seed)
    g.make_node(NumberlineView(AllNodesOfClass(Number)))
    g.make_node(NumberlineView(AllNodesTagged(Avail)))
    return g

def irun(numble=Numble([4, 5, 6], 15), seed=None):
    '''Sets up interactive use in 'py -i numbo3.py' '''
    global g
    g = new_graph(numble=numble, seed=seed)
    g.update_coarse_views()

if __name__ == '__main__':
    irun()
    pg(g)

#    from NumberLine import NumberLine
#    nl = NumberLine(ValueOf(g))
#    nl.add(*g.nodes_with_tag(Avail))
#    eyes = [nl.make_eye() for i in range(10)]
#    for eye in eyes:
#        print(eye, eye.look())
