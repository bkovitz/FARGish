# Tags.py -- Class definitions for tags: nodes that describe other nodes

from __future__ import annotations
from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, IO, \
    runtime_checkable

from FMTypes import Node, Ref, R
from Features import Feature
from util import short, as_dict, dict_str


@dataclass(frozen=True)
class DeadEnd:
    for_goal: R[Node] = Ref('for_goal')

    # TODO Inherit from something that has this.
    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({dict_str(as_dict(self), xform=short)})'
