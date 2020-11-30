
import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Callable

from Ac import All, OfClass, Acs, empty_env, Tagged, AllAre
from StdGraph import Graph, pg 
from Numble import make_numble_class
from testNodeClasses import *
from Action import Action
from Node import Node, NRef


class Workspace(Node):
    pass

class TestAc(unittest.TestCase):

    def test_simple_ac(self):
        Numble = make_numble_class(
            Brick, Target, Want, Avail, Allowed, [Plus, Times]
        )
        g = Graph(port_mates=port_mates)
        ws = g.add_node(Workspace)
        numble = Numble([4, 5, 6], 15)
        numble.build(g, ws)

        find_all_bricks = Acs(
            All(OfClass(Brick), within=ws), #AllAre(Tagged(Avail)), TagWith(AllBricksAvail)
        )

        find_that_all_bricks_are_avail = Acs(
            All(OfClass(Brick), within=ws), AllAre(Tagged(Avail)), #TagWith(AllBricksAvail)
        )

        targetid = 1  # HACK
        #env, result = find_all_bricks.do(g, targetid, empty_env)
        env, result = find_that_all_bricks_are_avail.do(g, targetid, empty_env)

        pg(g) #DEBUG
        print('ENV', env)
        print('RESULT', result)
        
