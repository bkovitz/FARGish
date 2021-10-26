
from __future__ import annotations
from dataclasses import dataclass, field, replace
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, \
    Iterator, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable

from Canvas import CellRef
from FMTypes import Value, Node
from FARGModel import Agent, Codelets, R, Ref, CellRef, Wake
from Codelets import Consume, Paint, BuildLitPainter, QuerySlipnetForDelegate, \
    Sleep, Build, NewState
from Detectors import AvailDetector
from QArgs import QBeforeFromAvails, QAfter, SearchFor
from Canvas import Operator
from Graph import Before, After
from util import trace, as_iter, pr, pts


@dataclass(frozen=True)
class LitPainter(Agent):
    value: Optional[Value] = None
    dest: Optional[CellRef] = None

    wake: Codelets = Paint()

@dataclass(frozen=True)
class Consumer(Agent):
    Q = TypeVar('Q', bound='Consumer')

    operator: Union[Operator, None] = None
    operands: Union[Tuple[Value, ...], None] = None
    source: Union[CellRef, None] = None  # where to get operands
    # TODO rm dest?
    #dest: Union[CellRef, None] = None    # where to paint result

    wake: Codelets = (
        Consume(
            operator=Ref('operator'),
            operands=Ref('operands'),
            source=Ref('source'),
            result_in='result'
        ),
        BuildLitPainter(value=Ref('result')),
        Sleep(agent=Ref('behalf_of'))
    )
    # Another possible approach, breaking down Consume into smaller codelets:
        #TakeOperands(operands=Ref('operands'), cellref=Ref('source')),
        #ComputeResult(),

    def features_of(self) -> Iterable[Node]:
        for operand in as_iter(self.operands):
            yield Before(operand)
        if self.operator:
            yield self.operator
        if self.operands and self.operator:
            result = self.operator(*self.operands)
            yield After(result)

    @classmethod
    def make(
        cls: Type[Q],
        operator: Union[Operator, None],
        operands: Union[Tuple[Value, ...], None]
    ) -> Q:
        return cls(operator=operator, operands=operands)
        
    @classmethod
    def make_table(
        cls,
        rands1: Iterable[int],
        rands2: Iterable[int],
        rators: Iterable[Operator]
    ) -> Iterable['Consumer']:
        for rand1 in rands1:
            for rand2 in rands2:
                for rator in rators:
                    if rand1 >= rand2:
                        result = rator(rand1, rand2)
                        if result != rand1 and result != rand2:
                            yield cls(operator=rator, operands=(rand1, rand2))

@dataclass(frozen=True)
class Want(Agent):
    startcell: R[CellRef] = Ref('startcell')
    target: R[Value] = Ref('target')
    on_success: R[Codelets] = Ref('on_success')

    born: Codelets = (
        Build(AvailDetector()),
        NewState(agent=Ref('behalf_of'), state=Wake)
    )
    wake: Codelets = (
        QuerySlipnetForDelegate(
            qargs=(
                QBeforeFromAvails(Ref('startcell')),
                QAfter(Ref('target')),
                SearchFor(Agent)
            )
        ),
        Sleep(agent=Ref('behalf_of'))
    )
