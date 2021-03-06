# brute.py -- Brute-force numble solver in FARGish
#
# Suitable for copying as a start for a cognitive model.
#
# brute.py is tested by testBrute.py. brute.py exercises most of the FARGish
# system and is also pretty simple, so it serves as a nice "unit" test to
# verify that the whole system works.

from operator import add, mul
from functools import reduce

#from PortGraph import PortGraph, pg, ps
from StdGraph import Graph, pg
from Action import Action, Fail, Raise
from Numble import make_numble_class
from codegen import make_python, compile_fargish
from log import *
from exc import FargDone
import expr
from ExprAsEquation import ExprAsEquation

prog = '''
gfuncs { succeeded }
funcs { ConsumeOperandsAction }

tags -- taggees
consume_operand -- proposer
proposed_operator -- proposer
behalf_of -- agents
target -- tags
source -- consumer

Tag(taggees)
Avail, Consumed, Failed, Done, Allowed, Promising, Hopeless : Tag
GettingCloser : Promising
Blocked(reason) : Tag

Workspace

Number(value)
Brick, Target, Block : Number

Operator
Plus, Times : Operator
Minus(minuend, subtrahend) : Operator

Want : Tag
  agent: OperandsScout(target=taggees)
  agent: SuccessScout(target=taggees)

OperandsScout(target)
  see p1 := NodeWithTag(Number, Avail),
      p2 := NodeWithTag(Number, Avail),
      op := NodeWithTag(Operator, Allowed)
  => build ConsumeOperands(op, p1, p2)
  else see block := NodeWithTag(Block, Avail), \
                    block != target, \
                    block.value != target.value
  => Fail(block)

SuccessScout(target)
  see winner := NodeWithValue(target.value, nodeclass=Number, tagclass=Avail)
  => succeeded(winner, target)

ConsumeOperands(proposed_operator, consume_operand, consume_operand)
  see Not(Tagged(Done, this)), AllTagged(Avail, consume_operand(this))
  => ConsumeOperandsAction()
'''

#make_python(prog, debug=1)  # Uncomment this to see generated code
exec(compile_fargish(prog, saveto='brute.gen.py'), globals())

def fail(self, g, thisid): #HACK
    #print('BLOCKFAIL', g.nodestr(thisid))
    for builder in g.neighbors(thisid, port_label='built_by'):
        g.datum(builder).fail(g, builder)
Block.fail = fail

Plus.expr_class = expr.Plus #HACK
Plus.symbol = '+' #HACK
Times.expr_class = expr.Times #HACK
Times.symbol = '*' #HACK

@classmethod
def consume_operands_fail(cls, g, thisid):
    '''Failure for ConsumeOperandsAction.'''
    built_number_ids = g.neighbors(
        thisid, port_label='built', neighbor_class=Number
    )
    operand_ids = g.neighbors(
        thisid, port_label='consume_operand'
    )
    #print('CONSUME_OPERANDS_FAIL') #DEBUG
    #if g.all_have_tag(Avail, built_number_ids):
    if g.has_tag(built_number_ids, Avail):
        g.move_tag(Avail, built_number_ids, operand_ids)
        g.remove_tag(operand_ids, Consumed)
    g.add_tag(Failed, thisid)
    for built_id in g.neighbors(thisid, neighbor_label='proposer'):
        g.add_tag(Failed, built_id)
ConsumeOperands.fail = consume_operands_fail

class ConsumeOperandsAction(Action):

    def go(self, g, actor):
        thisid = actor  # TODO simplify this out?
        op_class = g.class_of(
            g.neighbor(thisid, port_label='proposed_operator')
        )
        operand_ids = g.neighbors(
            thisid, port_label='consume_operand'
        )
        op_id = g.add_node(op_class, built_by=thisid)
        for operand_id in operand_ids:
            g.add_edge(op_id, 'operands', operand_id, 'consumer')
        result_id = g.add_node(
            #Block(arith_result(g, op_id)), built_by=thisid
            Block(arith_result(g, op_id)), built_by=thisid, source=op_id
        )
        #g.add_edge(result_id, 'source', op_id, 'consumer')
        g.move_tag(Avail, operand_ids, result_id)
        g.add_tag(Consumed, operand_ids)
        g.add_tag(Done, thisid)
        self.annotation_string = f"Trying {str(extract_expr(g, result_id))}"

def arith_result(g, operator_id):
    operator_class = g.class_of(operator_id)
    #print('OCLASS', operator_class)
    if operator_class == Minus: #HACK
        return 0.0  # STUB
    else:
        operand_ids = g.neighbors(operator_id, port_label='operands')
        return arith_result0(g, operator_class, operand_ids)

def arith_result0(g, operator_class, operand_ids):
    operand_values = [g.value_of(o) for o in operand_ids]
    # TODO It would be much better if FARGish let you define these operations
    # as class attributes.
    if operator_class is None:
        return None
    elif operator_class == Plus:
        return reduce(add, operand_values, 0)
    elif operator_class == Times:
        return reduce(mul, operand_values, 1)
    else:
        #raise ValueError(f'Unknown operator class {operator_class} of node {operator_id}.')
        raise ValueError(f'Unknown operator class {operator_class}.')

class NumboSuccess(FargDone):
    succeeded = True

    def __init__(self, expr):
        self.expr = expr

    def __str__(self):
        return 'Success!  ' + str(self.expr)

def succeeded(g, winnerid, targetid):
    return Raise(NumboSuccess,
                 expr.Equation(
                   extract_expr(g, winnerid),
                   extract_expr(g, targetid)))

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

Numble = make_numble_class(
    Brick, Target, Want, Avail, Allowed, [Plus, Times]
)

class BruteGraph(ExprAsEquation, Graph):

    def __init__(self, numble=None, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.port_mates += port_mates
        self.nodeclasses.update(nodeclasses)
        self.add_nodeclasses(Numble)

        ws = self.add_node(Workspace)
        if numble:
            numble.build(self, ws)
        # TODO Put in a WithActivation.Propagator

def new_graph(numble, seed=None):
    g = BruteGraph(numble=numble, seed=seed)
    return g

g = None

def run(seed=None, numble=Numble([4, 5, 6], 15), num=70):
    global g
    g = new_graph(seed=seed, numble=numble)
    #print('SEED', g.graph['seed'])
    #start_logging([ShowActionList, ShowActionsChosen])
    #pg(g)
    g.do_timestep(num=num)
    return g

if __name__ == '__main__':
    ShowAnnotations.start_logging()
    ShowActiveNodes.start_logging()
    ShowActionList.start_logging()
    ShowActionsChosen.start_logging()

    run(seed=4730533389549952010, num=40)
    pass
