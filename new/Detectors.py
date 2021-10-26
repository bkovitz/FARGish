# Detectors.py

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable

from FARGModel import FARGModel, Detector, R, Ref, Value, CellRef, Codelets


@dataclass(frozen=True)
class AvailDetector(Detector):
    target: R[Value] = Ref('target')
    startcell: R[CellRef] = Ref('startcell')
    on_success: R[Codelets] = Ref('on_success')

    def look(  # type: ignore[override]
        self,
        fm: FARGModel,
        target: Value,
        startcell: CellRef,
        on_success: Codelets
    ) -> Codelets:
        while True:
            if startcell.has_avail_value(target):
                return on_success
            else:
                startcell = startcell.next_cellref()
                if startcell.value is None:
                    break
        return None
            