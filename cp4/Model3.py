# Model3.py -- Starting from "FakeIt" at the top level, growing flexibility

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check, get_type_hints, get_args
from abc import ABC, abstractmethod


def run(problem) -> str:
    ws = problem_to_ws(problem)
    
    while True:
        ws = process_A(ws)
        ws = process_B(ws)
        if time_to_stop(ws):
            break
    return ws.solution()


def process_A(ws: Workspace) -> Workspace:
    new_ws_objs = ws.run_detectors()
    return ws.add_objs(new_ws_objs)

def process_B(ws) -> Workspace:
    p = choose_painter(ws)
    return p.run(ws)

# NEXT FakeIt to get ab_ working
# THEN incorporate other elements: unify, PainterCluster, PCMaker


def problem_to_ws(s: str) -> Workspace:
    return Workspace().define_and_name(Canvas.from_str(s))

def time_to_stop(ws: Workspace) -> bool:
    return ws.t >= 5

def choose_painter(ws: Workspace) -> Painter:
    return NullPainter()

class Painter(ABC):
    
    @abstractmethod
    def run(self, ws: Workspace) -> Workspace:
        pass

class NullPainter(Painter):

    def run(self, ws: Workspace) -> Workspace:
        return ws.bump_t()

##### CanvasObjects #####

@dataclass(frozen=True)
class Letter:
    '''A letter in the range a..z.'''
    c: str

    def __post_init__(self):
        if len(self.c) != 1 or self.c < 'a' or self.c > 'z':
            raise ValueError(f"Letter {self.c!r}: must be in range 'a'..'z'.")

    @classmethod
    def from_str(cls, c: str) -> Union[Letter, Blank]:
        if len(c) != 1:
            raise ValueError('Letter.from_str(): {c!r} must have len==1')
        if c == ' ' or c == '_':
            return Blank()
        else:
            return cls(c)
 
@dataclass(frozen=True)
class Blank:

    def __repr__(self) -> str:
        return self.__class__.__name__

    short = __repr__

    def __str__(self) -> str:
        return ' '

CanvasValue = Union[Letter, Blank]  # TODO change str to Letter

##### The Canvas #####

@dataclass(frozen=True)
class Canvas:
    contents: Tuple[CanvasValue, ...]

    @classmethod
    def from_str(cls, s: str):
        return Canvas(tuple(Letter.from_str(c) for c in s))
    
##### The Workspace #####

@dataclass(frozen=True)
class Workspace:
    t: int = 0   # timestep

    def solution(self) -> str:
        return 'abc'  #FAKE

    def bump_t(self) -> Workspace:
        return replace(self, t=self.t + 1)

    def define_and_name(self, obj) -> Workspace:
        return self #FAKE

    def run_detectors(self) -> Workspace:
        return self.bump_t()  #FAKE

    def add_objs(self, *objs) -> Workspace:
        return self  #FAKE


if __name__ == '__main__':
    solution = run('ab_')
    print(solution)
