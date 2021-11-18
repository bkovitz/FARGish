# Detectors.py

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable

from FMTypes import R, Ref
from FARGModel import FARGModel, Detector, Value, CellRef, Codelets
from Log import trace
from util import short, pr, pts, as_tuple


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
            
    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.target)}, {short(self.startcell)}, {short(self.on_success)})'

@dataclass(frozen=True)
class DeadEndDetector(Detector):
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
        lastcell = startcell.last_painted_cellref()
        avails = as_tuple(lastcell.avails)
        if len(avails) == 1 and avails[0] != target:
            return ({'dead_end': lastcell},) + as_tuple(on_success)
        else:
            return None

