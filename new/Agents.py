
from __future__ import annotations
from dataclasses import dataclass, field, replace
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, \
    Iterator, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable

from Canvas import CellRef
from FMTypes import Value, Node, R, Ref
from FARGModel import Agent, Codelets, CellRef, Wake, ExcludeExisting, \
    Desnag, ValuesNotAvail
from Codelets import Paint, BuildLitPainter, QuerySlipnetForDelegate, \
    Sleep, Build, NewState, MakeVariantFromAvails, ISucceeded
from Consume import Consume
from Detectors import AvailDetector
from QArgs import QBeforeFromAvails, QAfter, SearchFor
from Canvas import Operator
from Graph import Before, After
from Log import trace, lo
from util import as_iter, pr, pts, short


@dataclass(frozen=True)
class LitPainter(Agent):
    value: Optional[Value] = None
    dest: Optional[CellRef] = None

    wake: Codelets = (Paint(), ISucceeded())

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.value)}, {short(self.dest)})'

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
    delegate_succeeded: Codelets = ISucceeded()

    def features_of(self) -> Iterable[Node]:
        for operand in as_iter(self.operands):
            yield Before(operand)
        if self.operator:
            yield self.operator
        if self.operands and self.operator:
            result = self.operator(*self.operands)
            yield After(result)

    def short(self) -> str:
        cl = self.__class__.__name__
        s = short(self.operator).join(short(o) for o in as_iter(self.operands))
        return f'{cl}({s})'

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
                SearchFor(Agent),
                ExcludeExisting()
            )
        ),
        Sleep(agent=Ref('behalf_of'))
    )

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.target)}, {short(self.startcell)})'

@dataclass(frozen=True)
class VariantMakerFromAvails(Agent):
    agent: R[Agent] = Ref('agent')  # The agent to make a variant of
    cellref: R[CellRef] = Ref('source')
    avails: R[Tuple[Value, ...]] = Ref('avails')
        # These values were avail; indices match indices in seeker's request
    unavails: R[Tuple[Value, ...]] = Ref('unavails')
        # These values were unavail; indices match indices in seeker's request

    wake: Codelets = (
        MakeVariantFromAvails(
            Ref('agent'), Ref('source'), Ref('avails'), Ref('unavails')
        ),
        Sleep(agent=Ref('behalf_of'))
    )
        

    def features_of(self) -> Iterable[Node]:
        yield Desnag(ValuesNotAvail)
        # TODO agent, cellref, avails, unavails
