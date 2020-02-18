# demo2.py -- Demo with some but not all of numbo3b provided by FARGish

from operator import add, mul
from functools import reduce
from Action import Action, Build, Fail, Raise
from PortGraph import PortGraph, Node, pg
import PortGraph as PG
from bases import ActiveNode
from NodeSpec import NodeOfClass, NodeWithTag, NodeWithValue, HasSameValue, \
    And, Not, CartesianProduct, TupAnd, NotLinkedToSame, no_dups
from LinkSpec import LinkSpec
from ExprAsEquation import ExprAsEquation
from TimeStepper import TimeStepper
from exc import FargDone
import expr
import support
from codegen import make_python, compile_fargish
from util import as_iter, reseed, intersection

prog = '''
external { chooseSubclass, arithResult, succeeded, consumeOperands, Fail }

tags -- taggees
consume-operand -- proposer
proposed-operator -- proposer
behalf_of -- agents
target -- tags


Tag

Avail, Consumed, Failed, Done, Allowed : Tag

Number(n)
  value = n

Workspace

Brick, Target, Block : Number

Operator

Plus, Times : Operator

#Want : Tag
#  agent: OperandsScout(target=taggee)
#  agent: SuccessScout(target=taggee)

#OperandsScout(target)
#  see p1 := NodeWithTag(Number, Avail)
#      p2 := NodeWithTag(Number, Avail)
#      op := NodeWithTag(Operator, Allowed)
#  => build ConsumeOperands(op, p1, p2)
#  else block := NodeWithTag(Block, Avail), block != target
#  => Fail(block)
#
#SuccessScout(target)
#  see winner := NodeWithValue(target.value, nodeclass=Number, tagclass=Avail)
#  => succeeded(winner, target)
#
#ConsumeOperands(consume_operand, consume_operand, proposed_operator)
#  see Not(Tagged(Done, this)), AllTagged(Avail, consume-operand(this))
#  => consumeOperands(this)  # external func; complicated
'''

#make_python(prog)
exec(compile_fargish(prog), globals())

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

def fail(self, g, thisid): #HACK
    for builder in g.neighbors(thisid, port_label='builder'):
        g.datum(builder).fail(g, builder)
Block.fail = fail

Plus.expr_class = expr.Plus #HACK
Times.expr_class = expr.Times #HACK

class Want(Tag, ActiveNode):

    operands_scout_link = LinkSpec('agents', 'behalf_of')
    backtracking_scout_link = LinkSpec('agents', 'behalf_of')
    done_scout_link = LinkSpec('agents', 'behalf_of')

    def actions(self, g, thisid):
        targetid = g.taggee_of(thisid)
        s1 = None
        if not g.has_neighbor_at(
            thisid, 'agents', neighbor_class=OperandsScout
        ):
            s1 = Build(OperandsScout, [self.operands_scout_link], [thisid],
                       kwargs=dict(targetid=targetid))
        s2 = None
#        if not g.has_neighbor_at(
#            thisid, 'agents', neighbor_class=BacktrackingScout
#        ):
#            s2 = Build(BacktrackingScout, [self.backtracking_scout_link], [thisid], kwargs=dict(targetid=targetid))
        s3 = None
        if not g.has_neighbor_at(
            thisid, 'agents', neighbor_class=DoneScout
        ):
            s3 = Build(DoneScout, [self.done_scout_link], [thisid],
                       kwargs=dict(targetid=targetid))
#        s1 = Build.maybe_make(OperandsScout, behalf_of=thisid)
#        s2 = Build.maybe_make(
#            BacktrackingScout, behalf_of=thisid, targetid=targetid
#        )
#        s3 = Build.maybe_make(
#            DoneScout, behalf_of=thisid, targetid=targetid
#        )
        return [s1, s2, s3]

class OperandsScout(ActiveNode):

    # IDEA Break it down into more scouts: OperatorScout and OperandsScout.
    # OperatorScout chooses an operator and then starts an OperandsScout
    # weighted in a way that makes sense for the operator and target.
    # An OperandsScout chooses operands and then chooses an operator,
    # weighting probabilities to suit the operands.
    # STILL BETTER Let OperatorScout and OperandsScout form coalitions.

    # ANOTHER IDEA Multiple OperandsScouts, each looking at number nodes and
    # deciding how or whether to combine them into a group of operands.

    def __init__(self, targetid):
        self.targetid = targetid

    link_specs = [
        LinkSpec('proposer', 'consume-operand'),
        LinkSpec('proposer', 'consume-operand'),
        LinkSpec('proposer', 'proposed-operator')
    ]
    nodes_finder = CartesianProduct(
        NodeWithTag(Number, Avail),
        NodeWithTag(Number, Avail),
        NodeWithTag(Operator, Allowed),
        whole_tuple_criterion=TupAnd(
            no_dups,
            NotLinkedToSame(
                *[link_spec.old_node_port_label for link_spec in link_specs]
            )
        )
    )

    def actions(self, g, thisid):
        #TODO on-behalf-of ?
        node_tup = self.nodes_finder.see_one(g)
        print('NODE_TUP', node_tup)
        if node_tup is not None:
            return [Build(ConsumeOperands, self.link_specs, node_tup)]
#        cos_in_progress = list(
#            g.nodes_without_tag(Failed,
#                nodes=g.nodes_without_tag(Done,
#                    nodes=g.nodes_of_class(ConsumeOperands))
#            )
#        )
        cos_in_progress = [
            co for co in g.nodes_of_class(ConsumeOperands)
                if g.datum(co).can_go(g, co)
        ]
        print('COS', cos_in_progress)
        if cos_in_progress:
            return []
        # no operands to consume, so fail, i.e. trigger backtracking
        nodeid = NodeWithTag(Block, Avail).see_one(g)
        if g.value_of(nodeid) != g.value_of(self.targetid):
            return [Fail(nodeid)]

def arith_result(g, operator_id):
    operator_class = g.class_of(operator_id)
    operand_ids = g.neighbors(operator_id, port_label='operands')
    operand_values = [g.value_of(o) for o in operand_ids]
    print('ARITH', operand_ids)
    # TODO It would be much better if FARGish let you define these operations
    # as class attributes.
    if operator_class == Plus:
        return reduce(add, operand_values, 0)
    elif operator_class == Times:
        return reduce(mul, operand_values, 1)
    else:
        raise ValueError(f'Unknown operator class {operator_class} of node {operator_id}.')

class ConsumeOperands(ActiveNode):

    def actions(self, g, thisid):
        if self.can_go(g, thisid):
            return [self.MyAction(g, thisid)]

    def can_go(self, g, thisid):
        return (
            not g.has_tag(thisid, Done)
            and
            g.all_have_tag(Avail, self.my_operands(g, thisid))
        )

    @classmethod
    def my_operands(self, g, thisid):
        return g.neighbors(thisid, port_label='consume-operand')

    class MyAction(Action):
        threshold = 0.0 # 1.0
        #IDEA Let probability weight be support - threshold

        def __init__(self, g, thisid):
            self.thisid = thisid

        def go(self, g):
            op_class = g.class_of(
                g.neighbor(self.thisid, port_label='proposed-operator')
            )
            operand_ids = g.neighbors(
                self.thisid, port_label='consume-operand'
            )
            op_id = g.make_node(op_class, builder=self.thisid) #TODO container?
            for operand_id in operand_ids:
                g.add_edge(op_id, 'operands', operand_id, 'consumer')
            result_id = g.make_node(
                Block(arith_result(g, op_id)), builder=self.thisid
            )
            g.add_edge(result_id, 'source', op_id, 'consumer')
            g.move_tag(Avail, operand_ids, result_id)
            g.add_tag(Consumed, operand_ids)
            g.add_tag(Done, self.thisid)

    @classmethod
    def fail(cls, g, thisid):
        built_number_ids = g.neighbors(
            thisid, port_label='built', neighbor_class=Number
        )
        operand_ids = g.neighbors(
            thisid, port_label='consume-operand'
        )
        if g.all_have_tag(Avail, built_number_ids):
            g.move_tag(Avail, built_number_ids, operand_ids)
            g.remove_tag(operand_ids, Consumed)
        g.add_tag(Failed, thisid)
        for built_id in g.neighbors(thisid, port_label='built'):
            g.add_tag(Failed, built_id)
        

class NumboSuccess(FargDone):

    def __init__(self, expr):
        self.expr = expr

    def __str__(self):
        return 'Success!  ' + str(self.expr)

class DoneScout(ActiveNode):

    def __init__(self, targetid):
        self.targetid = targetid

    def actions(self, g, thisid):
        v = g.value_of(self.targetid)
        #node_ids = NodeWithTag(Number, Avail).see_all(g)
        #winner_id = next((g.value_of(id) == v for id in node_ids), None)
        winner_id = \
            NodeWithValue(v, nodeclass=Number, tagclass=Avail).see_one(g)
        if winner_id is not None:
            return [Raise(NumboSuccess,
                          expr.Equation(
                            extract_expr(g, winner_id),
                            extract_expr(g, self.targetid)))]


def extract_expr(g, nodeid):
    '''Extracts an Expr tree consisting of nodeid and its sources.'''
    nodeclass = g.class_of(nodeid)
    if issubclass(nodeclass, Block):
        return extract_expr(g, g.neighbor(nodeid, 'source'))
    elif issubclass(nodeclass, Number):
        return expr.Number(g.value_of(nodeid))
    elif issubclass(nodeclass, Operator):
        operand_exprs = (
            extract_expr(g, n)
                for n in g.neighbors(nodeid, ['source', 'operands'])
        )
        return g.datum(nodeid).expr_class(*operand_exprs)
    else:
        raise ValueError(f'extract_expr: node {nodeid} has unrecognized class {nodeclass}')

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
        Allowed.add_tag(g, plusid)
        Allowed.add_tag(g, timesid)
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
    g = DemoGraph(numble=numble, seed=seed)
    return g
    
g = None

def run(seed=None):
    global g
    g = new_graph(seed=seed, numble=Numble([4, 5, 6], 15))
    print('SEED', g.graph['seed'])
    #pg(g)
    g.do_timestep(num=70)
    #ConsumeOperands.fail(g, 23)
    #g.do_timestep()
    #g.do_timestep()
    #g.do_timestep()
    # Succeeds at last timestep with above seed and numble.

run(seed=8316664589534836549)
