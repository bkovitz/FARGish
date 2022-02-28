# An experiment: canvas-segments in a "soup", i.e. an unordered set

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING

from Log import trace, lo
from util import pts


Seg = str | List  # str  # A segment of a canvas.

def soup_matches(mem: Set[Seg], soup: Set[Seg]) -> Iterable[Overlap]:
    for item in soup:
        yield from item_matches(mem, item)

def item_matches(mem: Set[Seg], item: Seg) -> Iterable[Overlap]:
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

def lst(s: Seg) -> List:
    if isinstance(s, list):
        return s
    else:
        return list(s)

@dataclass
class Overlap:
    first: Seg
    second: Seg
    ifirst: int  # index of overlapping char
    isecond: int  # index of overlapping char

    def result(self) -> Seg:
        if isinstance(self.first, str) and isinstance(self.second, str):
            return self.first[:self.ifirst] + self.second[self.isecond:]
        else:
            return (
                lst(self.first)[:self.ifirst] + lst(self.second)[self.isecond:]
            )
    

def test1() -> None:
    mem: Set[Seg] = set(['jb'])

    ms = item_matches(mem, 'aj')
    for m in ms:
        print(f'{m}  {m.result()}')


mem: Set[Seg] = set(['aj', 'ja'])
soup: Set[Seg] = set(['[aj'])

ms = soup_matches(mem, soup)
for m in ms:
    print(f'{m}  {m.result()}')
