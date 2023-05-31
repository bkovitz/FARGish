# Model.py -- "Indexical" term-rewriting Copycat model

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check, get_type_hints, get_args
from dataclasses import dataclass, field, fields, replace, InitVar, Field, \
    astuple, is_dataclass


@dataclass(frozen=True)
class Canvas:
    s: str
    
    @classmethod
    def from_str(cls, s: str) -> Canvas:
        return Canvas(s)

    def span(self, lb: int, ub: int) -> Span:
        '''lb and ub are 1-based indices.'''
        return Span(self, lb, ub)

@dataclass(frozen=True)
class Span:
    canvas: Canvas
    lb: int
    ub: int

    def Chunk(self, *elems: ChunkElem) -> Chunk:
        '''Convenience function to make easily readable unit tests. That's
        why Chunk has its first letter in upper case.'''
        return Chunk(self, frozenset(elems))

@dataclass(frozen=True)
class Chunk:
    span: Span
    #body: Tuple[ChunkElem, ...]
    body: FrozenSet[ChunkElem]

class ChunkElem:
    '''Something that can go inside a Chunk.'''
    pass

@dataclass(frozen=True)
class Run(ChunkElem):
    rule: Any  #TODO type

    @classmethod
    def make_run_chunk(cls, span: Span) -> Optional[Chunk]:
        return span.Chunk(Run(succrule), Seed(a), Length(3))
        '''
        Look at relation between each pair of consecutive letters.
        TODO Searching for a Run really should really look at elements that
        could be Chunks or could be letters.

        If all those relations are the same, then make a Chunk.
        '''

@dataclass(frozen=True)
class Seed(ChunkElem):
    a: Any  # TODO type

@dataclass(frozen=True)
class Length(ChunkElem):
    n: int

@dataclass(frozen=True)
class Rule:
    '''A rewrite rule of the form lhs => rhs.'''
    lhs: Any  #TODO type
    rhs: Any  #TODO type

@dataclass(frozen=True)
class Succ:
    '''Ultimately, we will need code to match Succ[X] against something
    and to paint Succ[X] given X.'''
    arg: Any  # TODO type

    @classmethod
    def elems_to_rule(cls, left: Any, right: Any) -> Optional[Rule]:
        '''If 'right' is the successor of 'left', return a Rule for it.'''
        # TODO types for left, right
        #return None
        return Rule(L, Succ(L))

L = 'L'  # TODO Make this a Variable

a = 'a'
succrule = 'succrule'  #TODO
