# Tags.py -- Class definitions for tags: nodes that describe other nodes

from __future__ import annotations
from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, IO, \
    runtime_checkable

from FMTypes import Node, Ref, R
from Features import Feature


@dataclass(frozen=True)
class DeadEnd:
    for_goal: R[Node] = Ref('for_goal')
