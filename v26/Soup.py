# An experiment: canvas-segments in a "soup", i.e. an unordered set

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from abc import ABC, abstractmethod

from Log import trace, lo
from util import pts


Seg = str | Tuple  # str  # A segment of a canvas.

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

def as_tup(s: Seg) -> Tuple:
    if isinstance(s, tuple):
        return s
    else:
        return tuple(char_to_tup_member(a) for a in s)

unk_names = set(['X', 'Y', 'Z'])

def char_to_tup_member(c: str) -> str | Unknown:
    if c in unk_names:
        return Unknown(c)
    else:
        return c

@dataclass(frozen=True)
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
                as_tup(self.first)[:self.ifirst]
                +
                as_tup(self.second)[self.isecond:]
            )

@dataclass(frozen=True)
class Unknown:
    '''An unknown.'''
    name: str

X = Unknown('X')
    
#def is_match(pattern: Tuple, seg: Tuple) -> bool:
#    return is_matchE(pattern, seg, {})
#
#def is_matchE(pattern: Tuple, seg: Tuple, env: Dict[str, Any]) -> bool:
#    if not seg:
#        return True
#    if not pattern:
#        return False

class WhatToPaint(ABC):

    @abstractmethod
    def result(self) -> Seg:
        pass

@dataclass(frozen=True)
class Paint(WhatToPaint):
    '''Something to paint on a Seg.'''
    new: Seg      # what to overlay onto 'seg'
    seg: Seg
    istart: int   # index, relative to start of 'seg', of where to paint 'new'.
                  # 'istart' may be negative, meaning that 'new' should be
                  # prepended to 'seg'.
    @trace
    def result(self) -> Seg:
#        seg: Tuple
#        new: Tuple
#        if self.istart == 0:
#            return self.new
#        elif self.istart > 0:
#            seg = as_tup(self.seg)
#            new = as_tup(self.new)
#            return seg[:self.istart] + new
#        else:
#            seg = as_tup(self.seg)
#            new = as_tup(self.new)
#            return new

        result = []
        i = self.istart
        ni = 0   # index into self.new
        si = 0   # index into self.seg

        while True:
            if i < 0:
                result.append(self.new[ni])
            else:
                if ni < len(self.new):
                    result.append(self.new[ni])
                else:
                    if si >= len(self.seg):
                        break
                    else:
                        result.append(self.seg[si])
            i += 1
            ni += 1
            si += 1

        return tuple(result)
            
#        result = []
#        i = self.istart
#        j = 0  # index into self.new
#        while True:
#            if i < 0 or j < len(self.new):
#                result.append(self.new[j])
#            elif i < len(self.seg):
#                result.
#            i += 1
#            j += 1
#        return tuple(result)

paint_nothing = Paint((), (), 0)

def make_wtp(pattern: Seg, seg: Seg) -> WhatToPaint:
    '''Returns what 'pattern' should paint on/before/after 'seg'.'''
    pattern: Tuple = as_tup(pattern)
    seg: Tuple = as_tup(seg)
    if not pattern or not seg:
        return paint_nothing
    p1 = pattern[0]
    s1 = seg[0]
    if len(pattern) == len(seg):
        if pattern == seg:
            return Paint(pattern, seg, 0)
        else:
            return paint_nothing
    elif len(pattern) < len(seg):
        return paint_nothing # TODO
    elif len(pattern) == len(seg) + 1:
        if pattern[:len(seg)] == seg:
            return Paint(pattern, seg, 0)
        elif pattern[-len(seg):] == seg:
            return Paint(pattern, seg, -1)  # WRONG, TODO
        else:
            return paint_nothing
    else:
        return paint_nothing

@dataclass(frozen=True)
class PatternPainter:
    pattern: Tuple

    def is_match(self, seg: Seg) -> bool:
        xvalue: Any = None
        for a in seg:
            #if isinstance(a, X):
            pass
                
        return False

Painter = Union[Seg, PatternPainter]

def test1() -> None:
    mem: Set[Seg] = set(['jb'])

    ms = item_matches(mem, 'aj')
    for m in ms:
        print(f'{m}  {m.result()}')


def test2() -> None:
    mem: Set[Seg] = set(['aj', 'ja'])
    soup: Set[Seg] = set(['[aj'])

    ms = soup_matches(mem, soup)
    for m in ms:
        print(f'{m}  {m.result()}')


mem: Set[Painter] = set([PatternPainter((X, 'j', X))])

#print(is_match((X, 'j', X), tup('aj')))
cases = [
    ('a', 'a'),   # pattern, seg
    ('a', 'aj'),
    ('aj', 'a'),
    ('aj', 'j'),
    ('ja', 'a'),
    ('jjaa', 'a')
]
for cas in cases:
    p = make_wtp(*cas)
    print(cas, p, p.result())
#print(make_wtp('a', 'a'))
#print(make_wtp('a', 'aj'))
#print(make_wtp('aj', 'a'))
#print(make_wtp('aj', 'j'))
#print(make_wtp('ja', 'a'))
#print(make_wtp('jjaa', 'a'))
