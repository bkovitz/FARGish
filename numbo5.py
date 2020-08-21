# numbo5.py -- Rebuilding Numbo from scratch again, this time starting with
#              1 1 1 1 1; 5.

from codegen import make_python, compile_fargish
from TimeStepper import TimeStepper
from log import *
from util import as_iter, reseed, intersection
from PortGraph import PortGraph, Node, pg, ps
import support
from Numble import make_numble_class, prompt_for_numble
from ExprAsEquation import ExprAsEquation
from bases import ActiveNode
from Action import Action, Build3, make_build3
from BuildSpec import make_buildspec
from criteria import Tagged, HasValue, OfClass

prog = '''
tags -- taggees
target -- tags
members -- member_of

Workspace

Tag(taggees)
Want, Avail, Allowed : Tag
Count(value) : Tag

Group(members)
Glom : Group

Number(value)
Brick, Target, Block : Number

Operator
Plus, Times : Operator
Minus(minuend, subtrahend) : Operator
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

##### Nodeclasses defined in Python

#class GlomThem(Action):
#
#    def __init__(self, nodes_to_glom):
#        self.nodes_to_glom = nodes_to_glom
#        self.
#
#    def go(self, g):

is_number = OfClass(Number)
        
class SameNumberGlommer(ActiveNode):
    
    def actions(self, g, thisid):
        number_node = g.look_for(is_number)
        all_with_same_value = g.find_all(
            is_number, HasValue(g.value_of(number_node))
        )
        #return [make_build3(g, Glom, [all_with_same_value], {})]
        return Build3.maybe_make(g, Glom, [all_with_same_value], {})

class MemberCounter(ActiveNode):

    def actions(self, g, thisid):
        group_node = g.look_for(OfClass(Group))
        if group_node is None:
            return
        num_members = len(g.neighbors(group_node, port_label='members'))
        # TODO No action without group_node
        return [make_build3(g, Count, [], {
            'taggees': [group_node], 'value': num_members
        })]

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
        self.make_node(SameNumberGlommer)
        self.make_node(MemberCounter)

def new_graph(numble, seed=None):
    g = DemoGraph(numble=numble, seed=seed)
    return g
    
g = None

ShowAnnotations.start_logging()
ShowActionList.start_logging()
ShowActionsChosen.start_logging()
ShowIsMatch.start_logging()

if __name__ == '__main__':
    g = new_graph(Numble([1, 1, 1, 1, 1], 5), seed=8028868705202140491)
    #bspec = make_buildspec(g, Glom, [[4, 6, 8]], {})
    #bspec = make_buildspec(g, Glom, [], dict(taggees=[4, 6, 8]))
    #glom1 = bspec.build(g)
    #glom2 = bspec.build(g)  # Should be None
    #pg(g)
    #print(glom1, glom2)
    g.do_timestep(num=5)
    pg(g)
    cr = OfClass(Number)
