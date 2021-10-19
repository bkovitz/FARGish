
from __future__ import annotations
from dataclasses import dataclass, field, replace
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, \
    Iterator, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable

from Canvas import CellRef
from FMTypes import Value
from FARGModel import Agent, Codelets, Ref
from Codelets import Consume
from Canvas import Operator


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
        #TakeOperands(operands=Ref('operands'), cellref=Ref('source')),
        #ComputeResult(),

        #BuildLitPainter(value=Ref('result'), cellref=Ref('dest'), 
    )
