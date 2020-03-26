# demo3.py -- Numbo made by analogy with Jumbo

'''
Elements of expressions join into Gloms.
'''

from codegen import make_python, compile_fargish
from ExprAsEquation import ExprAsEquation
from TimeStepper import TimeStepper
from log import *
from PortGraph import PortGraph, Node, pg, ps
import support
from Numble import make_numble_class, prompt_for_numble
from util import as_iter, reseed, intersection

prog = '''
tags -- taggees
consume-operand -- proposer
proposed-operator -- proposer
target -- tags

Workspace

Tag(taggees)
Want, Allowed, Avail : Tag
CouldBeOperand : Tag

Number(value)
Brick, Target, Block : Number

Operator
Plus, Times : Operator
Minus(minuend, subtrahend) : Operator

OperandTagger
  see node := NodeOfClass((Brick, Block))
  => build CouldBeOperand(node)
'''

make_python(prog, debug=1)
exec(compile_fargish(prog), globals())

Numble = make_numble_class(
    Brick, Target, Want, Avail, Allowed, [Plus, Times, Minus]
)

##### Hacks

tag_port_label = 'taggees'
taggee_port_label = 'tags'

@classmethod
def cls_add_tag(cls, g, taggees):  # HACK
    taggees = list(as_iter(taggees))
    taggee_containers = intersection(
        *[g.member_of(ee) for ee in as_iter(taggees)]
    )
    tag = g.make_node(cls, container=taggee_containers)
    for taggee in as_iter(taggees):
        g.add_edge(tag, tag_port_label, taggee, taggee_port_label)
    return tag

Tag.add_tag = cls_add_tag  # HACK

##### The graph class and other generic execution code #####

class DemoGraph(TimeStepper, ExprAsEquation, PortGraph):

    port_mates = port_mates

    default_graph_attrs = dict(
        t=0,
        done=False,
        num_timesteps=40,
        seed=None,
        running=False,
        port_mates=port_mates,
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

        #HACK
        #self.make_node(LandmarkScout)
        self.make_node(OperandTagger)

def new_graph(numble, seed=None):
    g = DemoGraph(numble=numble, seed=seed)
    return g
    
g = None

ShowAnnotations.start_logging()
ShowActionList.start_logging()
ShowActionsChosen.start_logging()

def demo(seed=None, num=800):
    '''Run this for Doug.'''
    global g
    while True:
        numble = prompt_for_numble(Numble)
        if numble is None:
            break
        g = new_graph(seed=seed, numble=numble)
        print('\nSEED', g.graph['seed'])
        print()
        g.do_timestep(num=num)
        if not g.succeeded():
            print('Failed')

def run(seed=None, numble=Numble([4, 5, 6], 15), num=70):
    global g
    g = new_graph(seed=seed, numble=numble)
    print('SEED', g.graph['seed'])
    #start_logging([ShowActionList, ShowActionsChosen])
    #pg(g)
    g.do_timestep(num=num)
    #ConsumeOperands.fail(g, 23)
    #g.do_timestep()
    #g.do_timestep()
    #g.do_timestep()
    # Succeeds at last timestep with above seed and numble.

if __name__ == '__main__':
    #run(seed=8316664589534836549)
    run(seed=1725458333626496812, num=6)
    #run()
    #demo(seed=4730533389549952010)
    #run(seed=2524266053616371958, numble=Numble([10, 10, 1, 2, 3, 4], 100), n=12)
