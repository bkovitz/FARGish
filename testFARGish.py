# testFARGish.py -- Unit tests for FARGish.py

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass, field
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable
import operator

import matplotlib.pyplot as plt
#import netgraph

from FARGish import FARGModel, Workspace, SeqState, Top, caddr_of, \
    ValueNotAvail


@dataclass(frozen=True)
class Operator:
    func: Callable
    name: str

    def call(self, *operands: int) -> int:
        return self.func(*operands)

    def __str__(self):
        return self.name

plus = Operator(operator.add, '+')
times = Operator(operator.mul, 'x')
minus = Operator(operator.sub, '-')

@dataclass(frozen=True)
class Numble:
    bricks: Tuple[int]
    target: int
    operators: Set[Operator] = frozenset([plus, times, minus])

    @property
    def avails(self) -> List[int]:
        return self.bricks

    def __str__(self):
        bricks_str = ', '.join(str(b) for b in self.bricks)
        return f'from {bricks_str} make {self.target}'


class testFARGish(unittest.TestCase):

    def test_ws(self):
        fm = FARGModel()
        numble = fm.paint(None, Numble((4, 5, 6), 15))
        self.assertEqual(caddr_of(numble), (fm.ws, Top))
        self.assertEqual(numble, Numble((4, 5, 6), 15))
        self.assertCountEqual(fm.all_at(None), [Numble((4, 5, 6), 15)])
        #NEXT add something else to Top

    def test_seqstate(self):
        ss = SeqState((4, 5, 6))
        self.assertEqual(ss.avails, (4, 5, 6))

        taken, remaining = ss.take_avails([5, 4])
        self.assertCountEqual(taken, [4, 5])
        self.assertCountEqual(remaining, [6])

        with self.assertRaises(ValueNotAvail) as cm:
            taken, remaining = ss.take_avails([5, 400])
        self.assertEqual(cm.exception, ValueNotAvail(ss, 400))


    @unittest.skip('not implemented yet')
    def test_consume(self):
        ws = Workspace()
        canvas1 = ws.add(Top, SeqCanvas(Numble((4, 5, 6), 15)))
        consume1 = ws.add(Top, Consume(plus, (4, 5)))
        canvas2 = consume1.paint(ws, canvas1)
        self.assertEqual(ws.get((canvas1, 'cdr')), canvas2)
