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
        minusid = g.make_node(Minus, container)
        Allowed.add_tag(g, plusid)
        Allowed.add_tag(g, timesid)
        Allowed.add_tag(g, minusid)
        return container

    def as_dict(self):
        return { 'bricks': self.bricks, 'target': self.target }

class DemoGraph(TimeStepper, ExprAsEquation, PortGraph):

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
    
# TODO Fix code duplication with numble.py
def prompt_for_numble():
    '''Prompts the user to enter bricks and a target at the keyboard.
    Returns a Numble object, or None if user just hit Enter.'''
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

g = None

ShowAnnotations.start_logging()
ShowActionList.start_logging()
ShowActionsChosen.start_logging()

def demo(seed=None, num=800):
    '''Run this for Doug.'''
    global g
    while True:
        numble = prompt_for_numble()
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
    run(seed=1725458333626496812, num=3)
    #run()
    #demo(seed=4730533389549952010)
    #run(seed=2524266053616371958, numble=Numble([10, 10, 1, 2, 3, 4], 100), n=12)
