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
        p = pair(NodeWithTag(Number, Avail), NotAlready(ConsumeOperands))
        if not p:
            return None
        opClass = chooseSubclass(Operator, NotAlready(ConsumeOperands, p))
        if not opClass:
            return None
        return Build.maybe_make(ConsumeOperands, opClass, p)

class ConsumeOperands(ActiveNode):

    def __init__(self, operatorClass, operandids):
        self.operatorClass = operatorClass
        self.operandids = operandids

    def actions(self, g, thisid):
        return [self.action(...)]

    def action(self, g, thisid):
        
