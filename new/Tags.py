# Tags.py -- Class definitions for tags: nodes that describe other nodes

from __future__ import annotations
from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, IO, \
    runtime_checkable

from FMTypes import Node, Ref, R
from FARGModel import FARGModel, Sources, CellRef, Painter, Tag
from Features import Feature
from util import short, as_dict, dict_str


@dataclass(frozen=True)
class DeadEnd(Tag):
    for_goal: R[Node] = Ref('for_goal')

    # TODO Inherit from something that has this.
    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({dict_str(as_dict(self), xform=short)})'

    """
    def extend(self, fm: FARGModel, sources: Sources) -> None:
        '''Extend the taggees of this tag as far as possible. So, a DeadEnd
        that tags a CellRef should be extended to tag the CellRef's LitPainter
        and Consumer that produced the LitPainter.'''
        taggees = set(fm.taggees_of(self))
        pending: Set[Node] = set(taggees)
        new_taggees: Set[Node] = set()
        while pending:
            for old_node in pending:
                for new_node in self.also_needs_tagging1(fm, old_node):
                    if new_node not in taggees:
                        fm.add_tag(new_node, self)
                        new_taggees.add(new_node)
            taggees |= new_taggees
            pending = new_taggees
            new_taggees.clear()
    """

    @classmethod
    def also_tag(cls, fm: FARGModel, node: Node) -> Iterable[Node]:
        '''Returns nodes that also need tagging if 'node' is tagged, but does
        not recurse. So, if 'node' necessitates tagging node A, and node A
        necessitates tagging node B, then also_needs_tagging1 will return
        node A but not node B.'''
        if isinstance(node, CellRef):
            yield from fm.painters_of(node, node.value)
        elif isinstance(node, Painter):
            yield from fm.behalf_of(node)
