# QArgs.py

from __future__ import annotations
from dataclasses import dataclass, field, replace
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, \
    Iterator, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable
from abc import ABC, abstractmethod

from FMTypes import Node, Nodes, Pred
from FARGModel import FARGModel, QArg, QInput, QPred, CellRef, Ref, R
from Graph import Before, After
from util import trace, as_iter


@dataclass(frozen=True)
class QBeforeFromAvails(QInput):
    source: R[CellRef] = Ref('source')
    
    def items(  # type: ignore[override]
        self, source: CellRef
    ) -> Nodes:
        return [Before(a) for a in as_iter(source.avails)]

@dataclass(frozen=True)
class QAfter(QInput):
    features: R[Nodes] = Ref('features')

    def items(  # type: ignore[override]
        self, features: Nodes
    ) -> Nodes:
        return [After(a) for a in as_iter(features)]

@dataclass(frozen=True)
class SearchFor(QPred):
    type: R[Type[Node]] = Ref('type')

    def pred(  # type: ignore[override]
        self, type: Type[Node]
    ) -> Pred:
        return type
