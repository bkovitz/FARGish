# Soup.py -- 'Soup' and related classes

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from collections import defaultdict
from random import choice, choices, random
from io import StringIO

from Addrs import as_index
from Painters import Painter
#from Subst import Subst, bottom_subst, empty_subst
from Log import lo, trace
from util import Numeric, nf, short

@dataclass
class Soup:
    #painters: Set = field(default_factory=lambda: set())
    painters: Dict[Painter, Numeric] = field(
        default_factory=lambda: defaultdict(int)
    )  # map Painter to clarity
    #painters: List = field(default_factory=list)

    @classmethod
    def make_from(cls, painters: Iterable[Painter]) -> Soup:
        return Soup(defaultdict(int, ((p, 1) for p in painters)))

    def add(self, *painters: Painter) -> None:
        for p in painters:
            self.painters[p] += 1

    def remove(self, *painters: Painter) -> None:
        '''It is not an error to remove a painter that does not exist.'''
        for p in painters:
            if p in self.painters:
                del self.painters[p]

    def clarity(self, p: Painter) -> Numeric:
        return self.painters.get(p, 0.0)

    def decay(self, factor=0.9) -> None:
        for p in self.painters:
            self.painters[p] *= factor

    def __contains__(self, p: Painter) -> bool:
        return p in self.painters

    def __iter__(self) -> Iterator[Painter]:
        return iter(self.painters)

    def clear(self) -> None:
        self.painters.clear()

# Commented out to prevent circular import (of Subst), and since nothing else
# seems to call these functions.
#    def matching_painters(self, xp: Painter) -> List[Tuple[Subst, Painter]]:
#        result = []
#        for p in self.painters:
#            subst = self.is_match(xp, p)
#            if subst:
#                result.append((subst, p))
#        return result
#
#    def is_match(self, xp: Painter, p: Painter) -> Subst:
#        '''Viewing xp as a painter template (possibly with variables that
#        need to be filled in), does p match xp?
#
#        Returning a BottomSubst() means no match.
#        '''
#        # TODO Require match of func, too
#        xi, xj, xf = xp
#        pi = as_index(p[0])
#        pj = as_index(p[1])
#        pf = p[2]
#
#        if xf == pf:
#            return empty_subst.unify(xi, pi).unify(xj, pj)
#        else:
#            return bottom_subst

    def has_painter(self, p: Painter) -> bool:
        return p in self.painters

    def choose(self) -> Painter:
        return choices(
            list(self.painters.keys()),
            list(self.painters.values())
        )[0]

    @classmethod
    def union(cls, *soups: Soup) -> Soup:
        # TODO What about clarities?
        #return Soup(union(*(soup.painters for soup in soups)))
        #return Soup(reduce(operator.add, (soup.painters for soup in soups), []))
        d: Dict[Painter, Numeric] = defaultdict(int)
        for soup in soups:
            for painter, clarity in soup.painters.items():
                d[painter] += clarity
        return Soup(d)

    def short(self) -> str:
        cl = self.__class__.__name__
        return cl

    def __str__(self) -> str:
        cl = self.__class__.__name__
        items = ', '.join(short(p) for p in self.painters)
        return f'{cl}({items})'

    def state_str(self) -> str:
        sio = StringIO()
        for pstr in sorted(
            f'{short(p)} {nf(cl)}' for p, cl in self.painters.items()
        ):
            print(pstr, file=sio)
        return sio.getvalue()

    __repr__ = state_str
