# Model3.py -- Starting from "FakeIt" at the top level, growing flexibility

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check, get_type_hints, get_args
from abc import ABC, abstractmethod

from util import force_setattr, short


def run(problem) -> Model:
    m = Model()
    m.run(problem)
    m.events = [  #FAKE
        Detected(Succ(at('C1', 1), at('C1', 2))),
        MadePainter('P1', Succ(at('C1', 1), at('C1', 2))),
        Detected(Succ(at('P1', 'left'), at('P1', 'right'))),
        MadePainter('P2',
            PainterCluster(
                Define('PP1', Succ(At('AA1'), At('AA2'))),
                Define('AA1', CanvasAddress('SS', 'II1')),
                Define('AA2', CanvasAddress('SS', 'II2')),
                Succ('II1', 'II2')
            )
        ),
        RanPainter('P2', AA1=CanvasAddress('C1', 2)),
        MadePainter('P3',
            Succ(At(CanvasAddress('C1', 2)), At(CanvasAddress('C1', 3)))
        ),
        RanPainter('P3')
    ]
    return m

@dataclass
class Model:
    ws: Workspace = field(default_factory=lambda: Workspace())
    events: List[Event] = field(default_factory=lambda: [])

    def run(self, problem) -> str:
        self.ws = problem_to_ws(problem)
        
        while True:
            self.ws = process_A(self.ws)
            self.ws = process_B(self.ws)
            if self.time_to_stop():
                break
        return self.ws.solution()

    def time_to_stop(self) -> bool:
        return self.ws.t >= 5

    def solution(self) -> str:
        return self.ws.solution()


def process_A(ws: Workspace) -> Workspace:
    new_ws_objs = ws.run_detectors()
    return ws.add_objs(new_ws_objs)

def process_B(ws: Workspace) -> Workspace:
    p = choose_painter(ws)
    return p.run(ws)

# NEXT FakeIt to get ab_ working
# THEN incorporate other elements: unify, PainterCluster, PCMaker


def problem_to_ws(s: str) -> Workspace:
    return Workspace().define_and_name(Canvas.from_str(s))

def choose_painter(ws: Workspace) -> Painter:
    return NullPainter()

##### Variables and Parameters #####

T = TypeVar('T')

@dataclass(frozen=True)
class Var:
    '''A variable, together with a level number so that the model can tell
    local variables in PainterClusters apart from variables of the same name in
    an enclosing scope.'''
    name: str
    level: int

    @classmethod
    def at_level(cls, v: Union[str, Var, T], level: int) -> Union[Var, T]:
        match v:
            case str() if is_variable(v):
                return Var(v, level)  # type: ignore[arg-type]
            case Var():
                return Var(v.name, level)
#            case CompoundWorkspaceObj():
#                return v.with_var_level(level)  # type: ignore[return-value]
            case _:
                return v  # type: ignore[return-value]
        return v  # type: ignore[return-value]   # mypy bug

    @classmethod
    def is_at_least_level_1(cls, v: Argument) -> bool:
        match v:
            case Var(level=level):
                return level >= 1
            case _:
                return False

    @classmethod
    def doubled_first_letter(cls, v: Variable, suffix: Optional[int]=None) \
    -> Variable:
        match v:
            case str():
                name = f'{v[0]}{v[0]}'
                if suffix is None:
                    return name
                else:
                    return name + str(suffix)
            case Var(name, level):
                new_name = name[0] + name[0]
                if suffix is not None:
                    new_name = new_name + str(suffix)
                return Var(new_name, level)

    @classmethod
    def append_suffix(cls, var: Variable, suffix: str) -> Variable:
        match var:
            case str():
                return var + suffix
            case Var(name, level):
                return Var(name + suffix, level)
        
Variable = Union[str, Var]
Parameter = Union[Variable, T]
OptionalParameter = Union[Variable, T, None]

ParameterName = Literal['left', 'right']

Index = int

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

##### Events #####

@dataclass(frozen=True)
class Event:
    pass

@dataclass(frozen=True)
class Detected(Event):
    what: Any

@dataclass(frozen=True)
class MadePainter(Event):
    name: Variable
    what: Any

@dataclass(frozen=True)
class RanPainter(Event):
    name: Variable
    args: Dict[str, Any]

    def __init__(self, name: Variable, **kwargs):
        force_setattr(self, 'name', name)
        force_setattr(self, 'args', kwargs)

##### Painters #####

class Painter(ABC):
    
    @abstractmethod
    def run(self, ws: Workspace) -> Workspace:
        pass

class NullPainter(Painter):

    def run(self, ws: Workspace) -> Workspace:
        return ws.bump_t()

@dataclass(frozen=True)
class Succ(Painter):
    left: Any
    right: Any

    def run(self, ws: Workspace) -> Workspace:
        raise NotImplementedError

@dataclass(frozen=True)
class PainterCluster(Painter):
    elems: Tuple[PainterClusterElem, ...]

    def __init__(self, *elems: PainterClusterElem):
        force_setattr(self, 'elems', elems)

    def __eq__(self, other: Any) -> bool:
        return (
            isinstance(other, PainterCluster)
            and
            set(self.elems) == set(other.elems)
        )

    def run(self, ws: Workspace) -> Workspace:
        raise NotImplementedError

##### Objects that go inside Painters #####

@dataclass(frozen=True)
class At:
    addr: Parameter[Address]

def at(container: str, index: Any) -> At:
    '''Shorthand for making an At, so you don't have to type CanvasAddress
    or ParameterAddress.'''
    return At(CanvasAddress(container, index))
    
@dataclass(frozen=True)
class Define:
    name: Variable
    value: Any

PainterClusterElem = Union[Define, Painter]

##### Addresses #####

@dataclass(frozen=True)
class CanvasAddress:
    canvas: Parameter[Canvas]
    i: Parameter[Index]

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.canvas)}, {self.i})'

    __repr__ = __str__

@dataclass(frozen=True)
class ParameterAddress:
    painter: Parameter[Painter]
    parameter_name: Parameter[ParameterName]  # Can't be indirect, i.e.
                                        # must be a variable, not a constant

    def __repr__(self) -> str:
        return f'PA({short(self.painter)}.{short(self.parameter_name)})'

    __str__ = __repr__

Address = Union[CanvasAddress, ParameterAddress]

##### Types and TypeGuards #####

def is_variable(x: Any) -> TypeGuard[Variable]:
    return (isinstance(x, str) and len(x) > 1) or isinstance(x, Var)

Argument = Any

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
