# testNodeClasses.py -- Common nodeclasses hard-coded in Python for unit tests
# and acceptance tests
#
# This enables unit tests to be written that create nodes in a PortGraph
# without needing to invoke any of the FARGish code-generation code.

from dataclasses import dataclass

from Node import Node
from PortMates import PortMates
from NodeParams import NodeParams, AttrParam, MateParam
from StdGraph import Graph, pg
from codegen import make_python, compile_fargish


'''
port_mates = PortMates([('taggees', 'tags'), ('target', 'tags')])

class Workspace(Node):
    pass
class Number(Node):
    node_params = NodeParams(AttrParam('value'))
class Brick(Number):
    is_duplicable = True
class Target(Number):
    pass
class Block(Number):
    pass
class Tag(Node):
    node_params = NodeParams(MateParam('taggees', 'tags'))
class Avail(Tag):
    pass
class Allowed(Tag):
    pass
class Want(Node):
    node_params = NodeParams(MateParam('target', 'tags'))
class Operator(Node):
    is_duplicable = True
class Plus(Operator):
    pass
class Times(Operator):
    pass
'''

prog = '''
tags -- taggees
within -- overriding
node1 -- overriding
node2 -- overriding
target -- overriding
consume_operands -- proposer
proposed_operator -- proposer
result_consumer -- source  # HACK: should be 'consumer'; see unique_mate().

Workspace

Tag(taggees)
Want : Tag
Avail, Consumed, Allowed, Done : Tag
SameValue, AllMembersHaveThisValue : Tag
Blocked(reason) : Tag
Failed(reason): Tag
Count(value) : Tag

Number(value)
Brick, Target, Block(source, consumer) : Number

Operator(operands, consumer)
Plus, Times : Operator
Minus(minuend, subtrahend) : Operator

Group(members)
Glom : Group
'''
exec(compile_fargish(prog, saveto='testNodeClasses.gen.py'), globals())

class NumboTestGraph(Graph):
    pass
