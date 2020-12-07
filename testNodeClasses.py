# testNodeClasses.py -- Common nodeclasses hard-coded in Python for unit tests
#
# This enables unit tests to be written that create nodes in a PortGraph
# without needing to invoke any of the FARGish code-generation code.

from dataclasses import dataclass

#from PortGraph import PortGraph, Node, pg
from Node import Node
from PortMates import PortMates
from NodeParams import NodeParams, AttrParam, MateParam


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
