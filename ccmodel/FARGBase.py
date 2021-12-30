# FARGBase.py -- The abstract base class for FARG models

from __future__ import annotations
from dataclasses import dataclass, field, replace
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from abc import ABC, abstractmethod

from FMTypes import CellContents
from ArgsMap import empty_args_map
if TYPE_CHECKING:
    from FMTypes import Node, N, Value, Pred
    from ArgsMap import ArgsMap
    from Canvas import NodeRef, Paintable, Cell, CellRef
    from Program import Program, ProgramResult
    from Tag import Tag

@dataclass
class FARGModelDataclassMixin:
    nodes: Set[Node] = field(default_factory=set)

    def has_node(self, node: Node) -> bool:
        return node in self.nodes

class FARGModel(ABC, FARGModelDataclassMixin):

    @abstractmethod
    def run(self, program: Program, args: ArgsMap=empty_args_map) \
    -> ProgramResult:
        pass

    # Node-related

    @abstractmethod
    def build(self, node_: Union[N, Type[N]], args: ArgsMap=empty_args_map) \
    -> N:
        pass

    @abstractmethod
    def build_instance(self, ntype: Type[N], args: ArgsMap=empty_args_map) -> N:
        pass

    @abstractmethod
    def node_query(self, node: Node, pred: Callable) -> Optional[CellRef]:
        pass

    @abstractmethod
    def deref(self, noderef: NodeRef) -> CellContents:
        pass

    @abstractmethod
    def avails_at(self, noderef: NodeRef) -> Tuple[Value, ...]:
        pass

    ### Painting-related ###

    @abstractmethod
    def paint(self, noderef: NodeRef, content: Paintable) -> Node:
        pass

    @abstractmethod
    def updated_content(self, old: CellContents, new: Paintable) \
    -> CellContents:
        pass

    ### Tag-related ###

    @abstractmethod
    # TODO x: CellContents?
    def has_tag(self, x: Union[Node, Cell, CellRef], pred: Pred) -> bool:
        pass

    @abstractmethod
    def add_tag(self, noderef: NodeRef, *tag: Tag) -> None:
        pass

    @abstractmethod
    # TODO rm? .add_tag() should put the tag on the cell if appropriate.
    def add_tag_to_cell(self, c: Union[Cell, CellRef], *tag: Tag) -> None:
        c.add_tag_to_cell(*tag)

    # Timestep-related

    @abstractmethod
    def do_timestep(self, run: Optional[Program]=None) -> Any:
        pass

