# Model3.py -- Starting from "FakeIt" at the top level, growing flexibility

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check, get_type_hints, get_args
from abc import ABC, abstractmethod

from pyrsistent import s as pset
from pyrsistent.typing import PSet

from Log import lo, trace
from util import force_setattr, short


def run(problem) -> Model:
    m = Model()
    m.run(problem)
    m.events += [  #FAKE
        #Detected(Succ(at('C1', 1), at('C1', 2))),
        #MadePainter('P1', Succ(at('C1', 1), at('C1', 2))),
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
    recently_detected: Set[Detected] = field(default_factory=set)
    t: int = 0  # current timestep

    def run(self, problem) -> str:
        self.ws = self.problem_to_ws(problem).add_detector(Succ)

        while True:
            self.do_timestep()
            if self.time_to_stop():
                break
        return self.ws.solution()

    def do_timestep(self) -> None:
        self.t += 1
        self.process_A()
        self.process_B()

    def process_A(self) -> None:
        #detections = list(self.run_detectors())
        #self.events += detections
        #self.ws = self.ws.add_objs(new_ws_objs)

        detection = self.choose_detection(list(self.run_detectors()))
        lo('PR_A', detection)
        if detection is not None:
            self.events.append(detection)
            self.recently_detected.add(detection)
            self.ws, name = self.ws.define_and_name(detection.what)
            self.events.append(MadePainter(name, detection.what))

    def process_B(self) -> None:
        p = self.choose_painter()
        self.ws = p.run(self.ws)

    def problem_to_ws(self, s: str) -> Workspace:
        ws, _ = Workspace().define_and_name(Canvas.from_str(s))
        return ws

    def choose_painter(self) -> Painter:
        #STUB
        return NullPainter()

    def choose_detection(self, detections: Collection[Detected]) \
    -> Optional[Detected]:
        # STUB This currently chooses the first Detected. A proper version
        # should choose randomly, based on weights. Also, recently_detected
        # should decay.
        lo('CHOO', detections)
        for d in detections:
            if d not in self.recently_detected:
                return d
        return None

    def time_to_stop(self) -> bool:
        return self.t >= 5  #FAKE

    def run_detectors(self) -> Iterable[Detected]:
        lo('RUND', self.ws.detectors)
        for detector in self.ws.detectors:
            for pair in self.ws.all_pairs():
                for painter in detector.examine_pair(self.ws, pair):
                    yield Detected(painter)

    def solution(self) -> str:
        return self.ws.solution()

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

    @classmethod
    def is_succ(cls, l1: Letter, l2: Letter) -> bool:
        try:
            return l1.succ() == l2
        except FizzleNoSucc:
            return False

    def succ(self) -> Letter:
        '''Raises FizzleNoSucc if this Letter has no successor.'''
        if self.c >= 'z':
            raise FizzleNoSucc
        else:
            return Letter(chr(ord(self.c) + 1))
 
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

    def __getitem__(self, i: Index) -> Optional[CanvasValue]:
        try:
            return self.contents[i - 1]
        except IndexError:
            return None

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

##### Detector #####

class DetectorClass(ABC):

    @classmethod
    @abstractmethod
    def examine_pair(cls, ws: Workspace, pair: Tuple[AtOrObj, AtOrObj]) \
    -> Iterable[Painter]:
        pass

Detector = Type[DetectorClass]

##### Painters #####

class Painter(ABC):
    
    @abstractmethod
    def run(self, ws: Workspace) -> Workspace:
        pass

class NullPainter(Painter):

    def run(self, ws: Workspace) -> Workspace:
        return ws

@dataclass(frozen=True)
class Succ(DetectorClass, Painter):
    left: Any
    right: Any

    def run(self, ws: Workspace) -> Workspace:
        raise NotImplementedError

    @classmethod
    def detect(self, ws: Workspace) -> Iterable[Detected]:
        yield Detected(Succ(at('C1', 1), at('C1', 2)))  #FAKE

    @classmethod
    def examine_pair(cls, ws: Workspace, pair: Tuple[AtOrObj, AtOrObj]) \
    -> Iterable[Painter]:
        #a1, a2 = pair
        #yield cls(At(a1), At(a2)) #FAKE
        #import pdb; pdb.set_trace()
        match pair:
            case (At(a1), At(a2)):
                if list(cls.examine_pair(ws, ws.deref_pair(pair))):
                #yield from cls.examine_pair(ws, ws.deref_pair(pair))
                    yield cls(*pair)
                #yield cls(*pair)
            case (Letter() as l1, Letter() as l2):
                lo('LETT', l1, l2)
                if Letter.is_succ(l1, l2):
                    yield cls(*pair)
            case _:
                raise NotImplementedError(pair)

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

##### Fizzles #####

class Fizzle(Exception):
    pass

@dataclass(frozen=True)
class WrongType(Fizzle):
    typ: Any  # What the type was supposed to be
    ref: Any  # The reference to the object of the wrong type
    obj: Any  # The object with the wrong type

@dataclass(frozen=True)
class CantDeref(Fizzle):
    addr: Any

@dataclass(frozen=True)
class FizzleNoSucc(Fizzle):
    pass

##### The Workspace #####

@dataclass(frozen=True)
class Workspace:
    detectors: PSet[Detector] = field(default_factory=pset)

    def solution(self) -> str:
        return 'abc'  #FAKE

    def define_and_name(self, obj) -> Tuple[Workspace, Variable]:
        return (self, 'P1') #FAKE

    def add_detector(self, detector: Detector) -> Workspace:
        return replace(self, detectors=self.detectors.add(detector))

    def add_objs(self, *objs) -> Workspace:
        return self  #FAKE

    def all_pairs(self) -> Iterable[Tuple[At, At]]:
        '''For all pairs to examine for relationships, returns the addresses
        of each element wrapped in Ats.'''
        yield (At(CanvasAddress('C1', 1)), At(CanvasAddress('C1', 2))) #FAKE

    def add_canvas(self, canvas: Canvas) -> Workspace:
        ws, _ = self.define_and_name(canvas)
        return ws

    def deref_pair(self, pair: Tuple[At, At]) -> Tuple[Obj, Obj]:
        match pair:
            case (At(addr1), At(addr2)):
                return (self.deref_addr(addr1), self.deref_addr(addr2))
            case _:
                raise NotImplementedError(pair)

    def deref_addr(self, addr: Parameter[Address]) -> Obj:
        match addr:
            case CanvasAddress(canvas, index):
                c = self.get_canvas(canvas)
                i = self.get_index(index)
                result = c[i]
                if result is None:
                    raise CantDeref(addr)
                return result
            case _:
                raise NotImplementedError(addr)

    def __getitem__(self, x: Argument) -> Optional[Argument]:
        if is_variable(x):
            return self.subst.get(x, None)
        else:
            return x

    def eval(self, expr: Optional[Argument]) -> Optional[AtOrObj]:
        match expr:
            case _ if is_variable(x):
                return self.eval(self[x])  # type: ignore[arg-type]
            case _:
                raise NotImplementedError(expr)

    def get_canvas(self, x: Parameter[Canvas]) -> Canvas:
        #return Canvas.from_str('axb') #FAKE
        match x:
            case Canvas():
                return x
            case _:
                result = self.eval(x)
                if isinstance(result, Canvas):
                    return result
                else:
                    raise WrongType(Canvas, x, result)
                raise NotImplementedError(x)

    def get_index(self, i: Parameter[Index]) -> Index:
        match i:
            case int():
                return i
            case _:
                raise NotImplementedError(i)

Obj = Union[Canvas, Painter, Address, CanvasValue, Index]
    # An Obj is a workspace object: anything that can be a value
AtOrObj = Union[At, Obj]
Argument = Union[AtOrObj, Variable]

if __name__ == '__main__':
    solution = run('ab_')
    print(solution)
