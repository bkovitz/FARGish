# Consume.py

from __future__ import annotations
from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable

from FMTypes import Value
from FARGModel import FARGModel, Codelet, Ref, R, CellRef, \
    CodeletResults
from Canvas import Operator


@dataclass(frozen=True)
class Consume(Codelet):
    operator: R[Operator] = Ref('operator')
    operands: R[Tuple[Value, ...]] = Ref('operands')
    source: R[CellRef] = Ref('source')
    result_in: R[str] = 'result'

    def run(  # type: ignore[override]
        self,
        fm: FARGModel,
        operator: Operator,
        operands: Tuple[Value, ...],
        source: CellRef,
        result_in: str
    ) -> CodeletResults:
        return dict([
            (result_in, operator.consume(source, operands)),
            ('dest', source.next_cellref())
        ])

