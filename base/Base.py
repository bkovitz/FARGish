# Base.py -- Base types, classes, and functions for FARGish
#
# These are collected into one file because they refer to each other,
# hence would cause circular imports if separated into multiple files.

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from abc import ABC, abstractmethod

from Pred import Pred, as_pred
from Log import lo, trace
from util import as_tuple


T = TypeVar('T')

TypeAnnotation = Any  # In lieu of a type annotation for 'type annotation'

# Values with absolute value < epsilon are treated as zero
epsilon = 0.001 # 0.00001

Value = Hashable
V = TypeVar('V', bound=Value)

# The address of a Canvas cell within its Canvas
Addr = int  #Hashable


# NEXT Define Node as a class, with all the common operations

@dataclass
class NodeDataclassMixin:
    datum: Value
    args: Optional[Dict[str, Value]] = None
    tags: Optional[Set[Tag]] = None

class Node(ABC, NodeDataclassMixin):

    @abstractmethod
    def add_tag(self, *tag: Tag) -> AddTagResult:
        '''Must return something that indicates whether the Node created
        a new Node (containing itself and the tag) or modified itself
        (putting the tag inside itself).'''
        pass

    @abstractmethod
    def has_tag(self, pred: Pred) -> bool:
        '''Is Node tagged with a tag that matches 'pred'?'''
        pass

    def is_directly_runnable(self) -> bool:
        return False

Nodes = Union[Node, Iterable[Node], None]
N = TypeVar('N', bound=Node)


@dataclass(frozen=True)
class NodeAlterationResult:
    '''What a FARGModel should do, after an operation that 'alters' a Node.
    Possibly nothing (if the Node is mutable and really got altered),
    possibly build a new node.'''
    # TODO
    pass

WSElem = Union[Node, Canvas]

@dataclass(frozen=True)
class ArgsMap:
    d: Dict[str, Value]

    def __add__(self, other) -> Node:
        return as_node(other).prepend(self)

def Avails(*vs: Value) -> ArgsMap:
    return ArgsMap(dict(avails=as_tuple(vs)))

@dataclass
class FARGModelDataclassMixin:
    nodes: Set[Node] = field(default_factory=set)
    canvases: Set[Canvas] = field(default_factory=set)

class FARGModel(ABC, FARGModelDataclassMixin):

    def build(self, elem: Union[Value, Node, Canvas]) -> WSElem:
        if isinstance(elem, Canvas):
            self.canvases.add(elem)
            return elem
        """
        else:
            node = as_node(elem)
        """
