# QArgs.py

from __future__ import annotations
from dataclasses import dataclass, field, replace
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, \
    Iterator, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable
from abc import ABC, abstractmethod

from FMTypes import Nodes
from FARGModel import FARGModel, QArg, CellRef, Ref, R
from Graph import Before, After
from util import trace, as_iter


@dataclass(frozen=True)
class BeforeFromAvails(QArg):
    source: R[CellRef] = Ref('source')
    
    def get_items(  # type: ignore[override]
        self, fm: FARGModel, source: CellRef
    ) -> Nodes:
        return [Before(a) for a in as_iter(source.avails)]
