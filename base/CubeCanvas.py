# CubeCanvas.py -- A Canvas that represents the perceived corners
#                  of a Necker cube

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING

from FARGModel import Canvas, Addr, Relation, RelationUnknownToCanvas, \
    Cell, CellContents, Value, Values
from Log import lo, trace
from util import as_set, as_iter, ps, pr


Horiz = Relation('Horiz')
Vert = Relation('Vert')
Diag = Relation('Diag')

@dataclass
class CubeCanvas(Canvas):
    _cells: List[Cell]
    jump_table: ClassVar[Dict[Tuple[int, Value], int]] = {
        (1, Horiz): 2,
        (1, Diag):  3,
        (1, Vert):  5,
        (2, Horiz): 1,
        (2, Diag):  4,
        (2, Vert):  6,
        (3, Horiz): 4,
        (3, Diag):  1,
        (3, Vert):  7,
        (4, Horiz): 3,
        (4, Diag):  2,
        (4, Vert):  8,
        (5, Horiz): 6,
        (5, Diag):  7,
        (5, Vert):  1,
        (6, Horiz): 5,
        (6, Diag):  8,
        (6, Vert):  2,
        (7, Horiz): 8,
        (7, Diag):  5,
        (7, Vert):  3,
        (8, Horiz): 7,
        (8, Diag):  6,
        (8, Vert):  4,
    }
    relations = (Horiz, Vert, Diag)

    instance_count: ClassVar[int] = 0
    
    def __init__(self):
        self._cells = [
            Cell(Addr(self, None, i)) for i in range(1, 9)
        ]

    def __post_init__(self) -> None:
        self.__class__.instance_count += 1

    def __hash__(self):
        '''This is necessary to maintain determinism.'''
        return hash(self.instance_count)

    def __getitem__(self, addr: Addr) -> CellContents:
        i = addr.index_as_int()
        if i < 1 or i > len(self._cells):
            raise NotImplementedError  # TODO raise specific exception
        return self._cells[i - 1].contents

    def __setitem__(self, addr: Addr, v: Value) -> None:
        i = addr.index_as_int()
        if i < 1 or i > len(self._cells):
            raise NotImplementedError  # TODO raise specific exception
        self._cells[i - 1].set_contents(v)

    def all_addrs(self) -> Iterable[Addr]:
        return (Addr(self, None, i) for i in range(len(self._cells)))

    # TODO mv to Canvas; OAOO
    def jump(self, addr: Addr, relation: Values) -> Set[Addr]:
        addrs = as_set(addr)
        for r in as_iter(relation):
            new_addrs = set()
            for addr in addrs:
                new_addrs |= self.single_jump(addr, r)
            addrs = new_addrs
        return addrs

    def single_jump(self, addr: Addr, relation: Value) -> Set[Addr]:
        if relation not in self.relations:
            raise RelationUnknownToCanvas(
                canvas=self, addr=addr, relation=relation
            )
        i = addr.index_as_int()
        try:
            return {replace(addr, index=self.jump_table[(i, relation)])}
        except KeyError:
            return set()

if __name__ == '__main__':
    from FARGModel import FARGModel
    fm = FARGModel()
    cu = fm.build(CubeCanvas())
