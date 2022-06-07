# Model.py -- The canvas-and-painters model

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from abc import ABC, abstractmethod

from util import Numeric, short, as_tuple


Value = Hashable
ValueTup = Tuple[Value, ...]
Func = Callable[[Value], Value]

@dataclass(kw_only=True)  # type: ignore[call-overload, misc]
class Canvas(ABC):
    MAX_CLARITY: Numeric = 6
    INITIAL_CLARITY: Numeric = 5

    """
    MAX_CLARITY: ClassVar[Numeric] = 6  # TODO move this to a dataclass or  # 5
                                        # maybe to RMem
    """
    @abstractmethod
    def all_addrs(self) -> Iterable[Addr]:
        pass
    
    @abstractmethod
    def as_tuple(self) -> ValueTup:
        pass

    @abstractmethod
    def __getitem__(self, addr: Addr) -> Value:
        pass

    @abstractmethod
    def __setitem__(self, addr: Addr, x: Value) -> None:
        pass

    @abstractmethod
    def has_addr(self, addr: Addr) -> bool:
        pass

    @abstractmethod
    def clarity(self, addr: Addr) -> Numeric:
        pass

    def all_clarities(self) -> Iterable[Numeric]:
        for addr in self.all_addrs():
            yield self.clarity(addr)

    @abstractmethod
    def set_clarity(self, addr: Addr, clarity: Numeric) -> None:
        pass

    @abstractmethod
    def addr_of(self, v: Value) -> DeterminateAddress:
        pass
        

@dataclass(frozen=True)
class DeterminateAddress:
    canvas: Canvas
    abs_addr: int

    def get_value(self) -> Value:
        return self.canvas[self.abs_addr]

    def set_value(self, v: Value) -> None:
        self.canvas[self.abs_addr] = v

    def __add__(self, operand: int) -> DeterminateAddress:
        return DeterminateAddress(self.canvas, self.abs_addr + operand)

@dataclass(frozen=True)
class MatchAddr:
    '''An Addr that matches a value and returns the DeterminateAddress of
    that value.'''
    v: Value    # value to match

    def to_determinate_address(self, env: Env) -> DeterminateAddress:
        # TODO Return an updated env?  Return multiple matches? Choose a match?
        return env.m.canvas.addr_of(self.v)

@dataclass
class Canvas1D(Canvas):
    contents: List[Value] #= field(default_factory=list)
        # Always supply a value for 'contents'! The default is only to
        # avoid an error for following MAX_CLARITY, which has a default.
    clarities: List[Numeric] = field(  # same # of elems as 'contents'
        default_factory=list,
        init=False
    )

    def __post_init__(self) -> None:
        self.clarities = [
            0 if x is None else int(self.INITIAL_CLARITY)   # - 1  * 0.61
                for i, x in enumerate(self.contents)
        ]

    @classmethod
    def make_from(cls, s: str) -> Canvas1D:
        return Canvas1D(contents=list(c if c != ' ' else None for c in s))

    def all_addrs(self) -> Iterable[Addr]:
        return range(1, len(self.contents) + 1)

    def as_tuple(self) -> ValueTup:
        return as_tuple(self.contents)

    def has_addr(self, addr: Addr) -> bool:
        if isinstance(addr, int):
            addr = addr - 1
            return addr >= 0 and addr < len(self.contents)
        else:
            return False

    def __getitem__(self, addr: Addr) -> Value:
        if isinstance(addr, int):
            try:
                return self.contents[addr - 1]
            except IndexError:
                return None
        else:
            return None

    def __setitem__(self, addr: Addr, x: Value) -> None:
        if isinstance(addr, int):
            addr = addr - 1
            if addr < 0:  # off the left edge of the canvas
                return
            if self.clarities[addr] == 0:
                try:
                    self.contents[addr] = x
                except IndexError:
                    # TODO Stretch the canvas?
                    return
                if x is not None:
                    self.clarities[addr] = 1
            elif x != self.contents[addr]:  # Trying to overwrite a value
                self.clarities[addr] -= 1
                if self.clarities[addr] <= 0:
                    self.contents[addr] = None
            else:  # Trying to write the value that is already there
                if self.clarities[addr] < self.MAX_CLARITY:
                    self.clarities[addr] += 1
        else:
            pass  # raise an exception?

    def clarity(self, addr: Addr) -> Numeric:
        if isinstance(addr, int):
            addr = addr - 1
            try:
                return self.clarities[addr]
            except IndexError:
                return self.MAX_CLARITY
        else:
            return 0  # raise an exception?

    def all_clarities(self) -> Iterable[Numeric]:
        return self.clarities

    def set_clarity(self, addr: Addr, clarity: Numeric) -> None:
        if isinstance(addr, int):
            addr -= 1
            self.clarities[addr] = clarity

    def addr_of(self, v: Value) -> DeterminateAddress:
        for i, x in enumerate(self.contents):
            if x == v:
                return DeterminateAddress(self, i + 1)
        # TODO raise Fizzle
        raise ValueError

    def __str__(self) -> str:
        items = ' '.join(short(x) for x in self.contents)
        citems = ' '.join(short(c) for c in self.clarities)
        return f'[{items}]'
        #return f'[{items}]{newline}[{citems}]'

    def short(self) -> str:
        return ''.join(
            ' ' if x is None else str(x)
                for x in self.contents
        )

@dataclass(frozen=True)
class RPainter:
    '''A Painter whose source is a value to match, and whose target is
    specified by spatial relation from where the source was found.'''
    source: MatchAddr
    target: OffsetAddr
    func: Func

    @classmethod
    def make(cls, v: Value, o: int, fn: Func) -> RPainter:
        return RPainter(MatchAddr(v), OffsetAddr('I', o), fn)

@dataclass(frozen=True)
class OffsetAddr:
    '''An Addr defined as an offset relative to a variable in an Env.'''
    varname: str
    o: int   # the number of cells of offset

    def to_determinate_address(self, env: Env) -> DeterminateAddress:
        return env.determinate_address(self.varname) + self.o

def set_value(dtarget: DeterminateAddress) -> None:
    pass

def get_value(dsource: DeterminateAddress) -> Value:
    pass

def source_of(p: Painter) -> Addr:
    if isinstance(p, tuple):
        return p[0]
    else:
        return p.source

def target_of(p: Painter) -> Addr:
    if isinstance(p, tuple):
        return p[1]
    else:
        return p.target

def func_of(p: Painter) -> Func:
    if isinstance(p, tuple):
        return p[2]
    else:
        assert isinstance(p, RPainter)
        #reveal_type(p)
        #reveal_type(p.func)
        return p.func  # type: ignore[return-value]  # mypy bug?


Addr = Union[int, MatchAddr, OffsetAddr]
Painter = Union[Tuple[Addr, Addr, Func], RPainter]


@dataclass
class Env:
    m: Model
    d: Dict[str, Value] = field(default_factory=dict)

    def to_determinate_address(self, varname, addr: Addr) -> None:
        self.d[varname] = self.m.to_determinate_address(addr, self)

    def determinate_address(self, varname) -> DeterminateAddress:
        v = self.d.get(varname, None)
        if isinstance(v, DeterminateAddress):
            return v
        else:
            raise ValueError   # TODO Fizzle

@dataclass
class Model:
#    lts: LongTermSoup
#    ws: WorkingSoup
    canvas: Canvas

    Q = TypeVar('Q', bound='Model')
#
#    def update(self) -> None:
#        self.run_painter(self.choose_painter())
#
    def run_painter(self, p: Painter) -> None:
        env = self.fresh_env()
        env.to_determinate_address('I', source_of(p))
        #dsource = self.to_determinate_address(source_of(p))
        #dtarget = self.to_determinate_address(target_of(p))
        #vin = dsource.get_value()
        env.to_determinate_address('J', target_of(p))
        vin = env.determinate_address('I').get_value()
        env.determinate_address('J').set_value(func_of(p)(vin))

#    def choose_painter(self) -> Painter:
#        pass

    def to_determinate_address(self, addr: Addr, env: Env) \
    -> DeterminateAddress:
        #STUB TODO
        if isinstance(addr, int):
            return DeterminateAddress(self.canvas, addr)
        else:
            assert isinstance(addr, MatchAddr) or isinstance(addr, OffsetAddr)
            return addr.to_determinate_address(env)  # TODO  , env ?

    def fresh_env(self) -> Env:
        return Env(self)

    @classmethod
    def make_from(cls: Type[Q], s: str) -> Q:
        return cls(canvas=Canvas1D.make_from(s))

    def __str__(self) -> str:
        return str(self.canvas)

    def short(self) -> str:
        return short(self.canvas)

def succ(v: Value) -> str:
    # TODO Deal with non-str
    return chr(ord(v) + 1)  # type: ignore[arg-type]

if __name__ == '__main__':
    #p = (1, 2, succ)
    p = RPainter.make('a', 1, succ)   # TODO  1 -> right1
    #c = Canvas1D.make_from('a  ')
    m = Model.make_from('a  ')
    m.run_painter(p)

    print(m)
