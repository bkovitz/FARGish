# An experiment: canvas-segments in a "soup", i.e. an unordered set

from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING

from Log import trace, lo
from util import pts


Seg = str  # A segment of a canvas.

def item_matches(mem: Set[Seg], item: Seg):
    for mem_item in mem:
        yield from ii_matches(mem_item, item)

def ii_matches(item1: Seg, item2: Seg):
    if item1[-1] == item2[0]:
        yield Overlap(item1, item2, ilast(item1), 0)
    if item2[-1] == item1[0]:
        yield Overlap(item2, item1, ilast(item2), 0)

def ilast(item: Seg) -> int:
    '''Returns index of last element of 'item'.'''
    return len(item) - 1

@dataclass
class Overlap:
    first: Seg
    second: Seg
    ifirst: int  # index of overlapping char
    isecond: int  # index of overlapping char

    def conjoin(self) -> Seg:
        return self.first[:self.ifirst] + self.second[self.isecond:]
    

mem = set(['jb'])
soup = set('aj')

ms = item_matches(mem, 'aj')
for m in ms:
    print(f'{m}  {m.conjoin()}')
