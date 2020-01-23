# numbo3b.py -- Manually "compiled" FARGish for brute-force numble solver

from PortGraph import Node, Tag
from util import setattr_from_kwargs

port_label_connections = {
    # TODO
}

class Avail(Tag):
    pass
class Consumed(Tag):
    pass
class Failed(Tag):
    pass

class Number(Node):
    def __init__(self, n)
        self.value = self.n
class Target(Number):
    pass
class Brick(Number):
    pass
class Block(Number):
    pass

class Operator(Node):
    pass
class Plus(Operator):
    pass
class Times(Operator):
    pass

class Want(ActiveNode):
    def __init__(self, targetid):
        self.targetid = targetid

    def actions(self, g, thisid):
        s1 = Build.maybe_make(OperandsScout, behalf_of=thisid)
        s2 = Build.maybe_make(
            BacktrackingScout, behalf_of=thisid, targetid=targetid
        )
        s3 = Build.maybe_make(
            DoneScout, behalf_of=thisid, targetid=targetid
        )
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
    def actions(self, g, thisid):
        #TODO on-behalf-of ?
        return Build.maybe_make(ConsumeOperands,
            # Put the Cartesian-combinatoric stuff in Build.maybe_make
            [('proposed-operand', 'proposal', NodeWithTag(Number, Avail)),
             ('proposed-operand', 'proposal', NodeWithTag(Number, Avail)),
             ('proposed-operator', 'proposal', NodeWithTag(Operator, Allowed))])

class ConsumeOperands(ActiveNode):

    def actions(self, g, thisid):
        return [self.MyAction(g, thisid)]

    class MyAction(Action):
        threshold = 1.0
        #IDEA Let probability weight be support - threshold

        def __init__(self, g, thisid):
            self.thisid = thisid

        def go(self, g):
            op_class = g.class_of(
                g.neighbor(self.thisid, port_label='proposed-operator')
            )
            operand_ids = g.neighbors(
                self.thisid, port_label='proposed-operand'
            )
            op_id = g.make_node(op_class) #TODO container?
            for operand_id in operand_ids:
                g.add_edge(op_id, 'operands', operand_id, 'consumer')
            # arith_result will follow the links from op_id to get the
            # operand values.
            result_id = g.make_node(Block(arith_result(g, op_id))
            move_tag(g, Avail, operand_ids, result_id)
            g.add_tag(g, Consumed, operand_ids)

class DoneScout(ActiveNode):

    def __init__(self, targetid):
        self.targetid = targetid

    def actions(self, g, thisid):
        v = g.value_of(self.targetid)
        node_ids = NodeWithTag(Number, Avail).find(g)
        winner_id = next(g.value_of(id) == v for id in node_ids, None)
        if winner_id:
            return Raise(DONE, winner_id)

class BacktrackingScout(ActiveNode):

    def __init__(self, targetid):
        self.targetid = targetid

    def actions(self, g, thisid):
        target_v = g.value_of(self.targetid)
        node_ids = Just1NodeWithTag(Number, Avail).find(g)
        if node_ids and g.value_of(node_ids[0]) != target_v:
            return self.MyAction(g, node_ids[0])

    class MyAction(Action):
        def __init__(g, node_id):
            #TODO
