# spike.py -- Architectural spike for FARGish/Numbo without a port graph

from dataclasses import dataclass, field
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable
from copy import copy
import operator

@dataclass(frozen=True)
class Operator:
    func: Callable
    name: str

    def call(self, *operands: int) -> int:
        return self.func(*operands)

plus = Operator(operator.add, '+')
times = Operator(operator.mul, '-')
minus = Operator(operator.sub, 'x')

@dataclass(frozen=True)
class Numble:
    bricks: List[int]
    target: int
    operators: Set[Operator] = frozenset([plus, times, minus])

@dataclass(frozen=True)
class SolnState:
    avails: List[int]
    last_move: Union[None, str] = None

    def move(self, operator: Operator, operands: List[int]) -> 'SolnState':
        avs = copy(self.avails)
        for operand in operands:
            avs.remove(operand)
            #TODO ValueError
        result = operator.call(*operands)
        expr = operator.name.join(str(n) for n in operands)
        return SolnState(
            [result] + avs,
            last_move=f'{expr}={result}'
        )

    def avails_str(self) -> str:
        return ' '.join(str(n) for n in self.avails)

    def __str__(self):
        astr = self.avails_str()
        if self.last_move:
            return ' '.join(str(x) for x in [self.last_move] + self.avails)
        else:
            return self.avails_str()

@dataclass(frozen=True)
class SolnCanvas:
    cells: List[SolnState]

    def move(self, operator: Operator, operands: List[int]) -> 'SolnCanvas':
        ss = copy(self.cells)
        # TODO Exception if self.cells is empty
        new_state = ss[-1].move(operator, operands)
        ss.append(new_state)
        return self.__class__(ss)

    @classmethod
    def init(cls, avails: List[int]) -> 'SolnCanvas':
        return cls([SolnState(avails)])

    def __str__(self):
        # TODO string when self.cells is empty
        return '; '.join(str(s) for s in self.cells)


numble = Numble([4, 5, 6], 15)
ss0 = SolnState([4, 5, 6])
ss1 = ss0.move(plus, [4, 5])

sc0 = SolnCanvas.init([4, 5, 6])
sc1 = sc0.move(plus, [4, 5])
sc2 = sc1.move(plus, [9, 6])

print(sc2)
