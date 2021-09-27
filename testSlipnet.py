# testSlipnet.py -- Unit tests for Slipnet.py

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass, field
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence
import operator
from operator import itemgetter, attrgetter
from collections import Counter

from Slipnet import Slipnet, Node, FeatureWrapper, IntFeatures
from util import is_iter, as_iter, pts, pr


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
class Equation(Node):
    operands: Tuple[int]
    operator: Operator
    result: int

    def __hash__(self):
        return hash((self.operands, self.operator, self.result))

    def features(self) -> Iterable[Hashable]:
        for operand in self.operands:
            yield operand
            yield Before(operand)
        yield self.operator
        yield self.result
        yield After(self.result)
        if all(self.result > operand for operand in self.operands):
            yield Increase()
        elif any(self.result < operand for operand in self.operands):
            yield Decrease()
        counter = Counter(self.operands)
        for operand, count in counter.items():
            if count == 2:
                yield Doubled(operand)
                yield Doubled()
        yield NumOperands(len(self.operands))
        mino = min(self.operands)
        maxo = max(self.operands)
        yield MinBefore(mino)
        yield MaxBefore(maxo)
        if mino == maxo:
            yield OneUniqueBefore(mino)
        elif set(range(mino, maxo + 1)) == set(self.operands):
            yield SequentialBefore(mino, maxo)

    def __str__(self):
        expr = f' {self.operator} '.join(str(n) for n in self.operands)
        return f'{expr} = {self.result}'

@dataclass(frozen=True)
class Increase:
    pass

@dataclass(frozen=True)
class Decrease:
    pass

class Before(FeatureWrapper):
    pass

class After(FeatureWrapper):
    pass

class MinBefore(FeatureWrapper):
    pass

class MaxBefore(FeatureWrapper):
    pass

class Doubled(FeatureWrapper):
    pass

class NumOperands(FeatureWrapper):
    pass

class OneUniqueBefore(FeatureWrapper):
    pass

@dataclass(frozen=True)
class SequentialBefore:
    lb: Any
    ub: Any

class UTSlipnet(IntFeatures, Slipnet):
    pass

class TestSlipnet(unittest.TestCase):

    @classmethod
    def make_slipnet(self):
        slipnet = UTSlipnet(
            Equation((a, b), operator, operator.call(a, b))
                for a in range(1, 11)
                for b in range(1, 11)
                for operator in [plus, times, minus]
                if a >= b
        )
        slipnet.add_layer2_nodes(
            Equation((a, 1), operator, operator.call(a, 1))
                for a in range(10, 102)
                for operator in [plus, minus]
        )
        slipnet.add_layer2_nodes(
            Equation((a, 2), plus, a + 2)
                for a in range(0, 102, 2)
        )

        '''
        slipnet.add_layer2_nodes([
            Equation((4, 5, 6), plus, 15)
        ])
        slipnet.add_layer2_nodes([40, 50, 60])
        slipnet.add_edge(Leading(4), 40, weight=1.0)
        slipnet.add_edge(Leading(5), 50, weight=1.0)
        '''
        return slipnet

    def test_slipnet_basics(self):
        slipnet = self.make_slipnet()
        e1 = Equation((5, 4), times, 20)
        e2 = Equation((5, 4), plus, 9)
        e3 = Equation((5, 4), minus, 1)

        q1 = slipnet.query(features=[4, 5], type=Equation, k=20)
        d1 = slipnet.to_d(q1)
        #pts(q1)
        #print()
        #print(d[e1])
        self.assertTrue(e1 in d1)
        self.assertTrue(e2 in d1)
        self.assertTrue(e3 in d1)

        q2 = slipnet.query(features=[Before(4), Before(5)], type=Equation, k=20)
        d2 = slipnet.to_d(q2)
        #pts(q2)
        self.assertTrue(e1 in d2)
        self.assertTrue(e2 in d2)
        self.assertTrue(e3 in d2)
        self.assertGreater(d2[e1], d1[e1])

        #print()
        q3 = slipnet.query(
            features=[Before(4), Before(5), After(15)], type=Equation, k=20
        )
        d3 = slipnet.to_d(q3)
        #pts(q3)
        self.assertTrue(e1 in d3)
        self.assertTrue(e2 in d3)
        self.assertTrue(e3 in d3)


if __name__ == '__main__':
    sl = TestSlipnet.make_slipnet()
    sl2 = TestSlipnet.make_slipnet()
    sl2.features_of = sl2.xfeatures_of # Oops, after construction is too late
                                       # for this
    top = Slipnet.top

    if False:
        e = Equation((5, 4), plus, 9)

        # "Backwash" test: will 40 and 50 receive much activation?
        q = sl.dquery([Equation((5, 4), plus, 9)])
        #pts(q)

        da = {
            4: 1.0,
            5: 1.0
        }
        t = sl.query(activations_in=da, type=Equation, k=20)
        pts(t)

    if True:
        # What happens if we pulse a non-existent feature node?

        fs = [Before(11)]
        d1 = sl.dquery(features=fs)
        d2 = sl2.dquery(features=fs)

        #pts(top(d1))
        print(sl.ns(Before(11)))
        print(sl2.ns(Before(11)))


