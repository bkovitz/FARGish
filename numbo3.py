# numbo3.py

from numble import Numble, prompt_for_numble
from PortGraph import PortGraph, NodeAndValue, pg, pn, ps, Node, Tag, CouldMake
from numbospec import *
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


def new_graph(numble, seed=None):
    return Numbo3Graph(numble=numble, seed=seed)

