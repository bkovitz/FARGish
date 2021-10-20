
from __future__ import annotations
from dataclasses import dataclass, field, replace
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, \
    Iterator, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable

from Canvas import CellRef
from FMTypes import Value
from FARGModel import Agent, Codelets, Ref, CellRef
from Codelets import Consume, Paint, BuildLitPainter
from Canvas import Operator


@dataclass(frozen=True)
class LitPainter(Agent):
    value: Optional[Value] = None
    dest: Optional[CellRef] = None

    wake: Codelets = Paint()

@dataclass(frozen=True)
class Consumer(Agent):
    operator: Union[Operator, None] = None
    operands: Union[Tuple[Value, ...], None] = None
    source: Union[CellRef, None] = None  # where to get operands
    dest: Union[CellRef, None] = None    # where to paint result

    wake: Codelets = (
        Consume(
            operator=Ref('operator'),
            operands=Ref('operands'),
            source=Ref('source'),
            result_in='result'
        ),

        BuildLitPainter(value=Ref('result'))
    )
    # Another possible approach, breaking down Consume into smaller codelets:
        #TakeOperands(operands=Ref('operands'), cellref=Ref('source')),
        #ComputeResult(),
