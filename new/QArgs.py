# QArgs.py

from __future__ import annotations
from dataclasses import dataclass, field, replace
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, \
    Iterator, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable
from abc import ABC, abstractmethod

from FMTypes import Node, Nodes, Pred, R
from FARGModel import FARGModel, QArg, QInput, QPred, CellRef, Ref
from Graph import Before, After
from Log import trace
from util import as_iter, short


# NEXT CanReplaceRefs
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

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.type)})'
