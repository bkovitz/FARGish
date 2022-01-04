# spike.py -- A spike to guide development of Base.py

from __future__ import annotations
#from Base import FARGModel, Canvas, Avails
#from Base import Plus
from util import ps, pr

from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from abc import ABC, abstractmethod
import operator

from Log import lo, trace
from util import as_tuple, short, as_dstr

Value = Hashable
CellContents = Value   # ?
Tag = Value  # TODO

@dataclass
class NodeDataclassMixin:
    datum: Value
    args: Optional[ArgsMap] = None
    tags: Optional[Set[Tag]] = None

class Node(NodeDataclassMixin):

    def with_args_merged(self, args: ArgsMap) -> Node:
        '''Returns new Node, with 'args' overriding self.args.'''
        return Node(self.datum, ArgsMap.merged(self.args, args), self.tags)

    def short(self) -> str:
        result = short(self.datum)
        if self.args:
            result += ' ' + short(self.args)
        if self.tags:
            result += ' ' + short(self.tags)
        return result

@dataclass(frozen=True)
class ArgsMap:
    '''A dictionary of argument names and their values, suitable for passing
    to a function.'''
    d: ArgsD

    def __add__(self, other) -> Union[ArgsMap, Node]:
        if isinstance(other, ArgsMap):
            return ArgsMap(other.d | self.d)
        elif isinstance(other, Node):
            return other.with_args_merged(self)
        else:
            return Node(other, self, None)

    def short(self) -> str:
        return ' '.join(f'{short(k)}={short(v)}' for k, v in self.d.items())

    @classmethod
    def merged(cls, new: Args, old: Args) -> Union[ArgsMap, None]:
        '''Returns an ArgsMap (or None) with the values from 'new' overriding
        the values from 'old'.'''
        if new is None:
            if old is None:
                return None
            elif isinstance(old, ArgsMap):
                return old
            else:
                return ArgsMap(old)
        elif old is None:
            if isinstance(new, ArgsMap):
                return new
            else:
                return ArgsMap(new)
        else:
            dnew = new.d if isinstance(new, ArgsMap) else new
            dold = old.d if isinstance(old, ArgsMap) else old
            return ArgsMap(dold | dnew)

ArgsD = Dict[str, Value]
Args = Union[None, ArgsD, ArgsMap]

def Avails(*vs: Value) -> ArgsMap:
    return ArgsMap(dict(avails=as_tuple(vs)))

@dataclass(frozen=True)
class Addr:
    canvas: Any  # TODO: declare specific types
    cpart: Optional[str] = None     # The relevant part of the canvas
    index: Optional[Value] = None   # which cell in that part

    def index_as_int(self) -> int:
        '''Returns the index as an int or raises an exception if that's
        not possible.'''
        if isinstance(self.index, int):
            return self.index
        else:
            raise NotImplementedError  # TODO raise specific exception

    def next(self) -> Addr:
        return replace(self, index=self.index_as_int()+1)

class Canvas:
    '''A Canvas is mutable. The contents of its cells may change, and the
    number of its cells may change.'''

    @abstractmethod
    def __getitem__(self, addr: Addr) -> CellContents:
        pass

    @abstractmethod
    def __setitem__(self, addr: Addr, v: Value) -> None:
        pass

@dataclass
class ActionCanvas(Canvas):
    '''An ActionCanvas contains two cparts: 'situation' and 'action'. Each
    'action' cell is an action to be done in the 'situation' described by
    the cell with the same index, and produces the following 'situation'
    cell.'''
    _situation_cells: List[Cell]
    _action_cells: List[Cell]

    instance_count: ClassVar[int] = 0
    
    def __post_init__(self) -> None:
        self.__class__.instance_count += 1

    def __hash__(self):
        '''This is necessary to maintain determinism.'''
        return hash(self.instance_count)

    def __getitem__(self, addr: Addr) -> CellContents:
        if addr.cpart == 'situation':
            cells = self._situation_cells
        elif addr.cpart == 'action':
            cells = self._action_cells
        else:
            raise NotImplementedError  # TODO raise specific exception
        i = addr.index_as_int()
        try:
            cell = cells[i]
        except IndexError:
            return None
            # TODO What if addr < 0?
        return cell.contents

    def __setitem__(self, addr: Addr, v: Value) -> None:
        i = addr.index_as_int()
        if i < 0:
            raise NotImplementedError  # TODO raise specific exception
        cell_list = self.get_cell_list(addr, i)
        cell_list[i].set_contents(v)

    def get_cell_list(self, which: Union[Addr, str], at_least: int=0) \
    -> List[Cell]:
        '''Returns the List[Cell] designated by 'which'. Ensures that it
        contains enough cells that the index 'at_least' is valid by creating
        new Cells that contain None.'''
        len_at_least = at_least + 1
        cpart_name = which.cpart if isinstance(which, Addr) else which
        if cpart_name == 'situation':
            result = self._situation_cells
        elif cpart_name == 'action':
            result = self._action_cells
        elif cpart_name is None:
            raise NotImplementedError  # TODO raise specific exception
        else:
            raise NotImplementedError  # TODO raise specific exception
        while len(result) < len_at_least:
            result.append(Cell(Addr(self, cpart_name, len(result)), None))
        return result

    @classmethod
    def make(cls, *cellcontents: CellContents) -> ActionCanvas:
        '''Makes a new ActionCanvas, filled in from 'cellcontents', alternating
        cparts between 'situation' and 'action', starting with 'situation'.'''
        result = cls.empty()
        for n, content in enumerate(cellcontents):
            if content is None:
                continue
            if n & 1 == 0:
                cpart = 'situation'
            else:
                cpart = 'action'
            result[Addr(result, cpart, n // 2)] = content
        return result

    @classmethod
    def empty(cls) -> ActionCanvas:
        return cls(_situation_cells=[], _action_cells=[])

    def short(self) -> str:
        l = max(len(self._situation_cells), len(self._action_cells))
        if l == 0:
            return '(empty ActionCanvas)'
        else:
            return ''.join(
                f"[ {short(self[Addr(self, 'situation', i)])} / {short(self[Addr(self, 'action', i)])} ]"
                    for i in range(l)
            )

@dataclass
class Cell:
    '''A Cell is mutable. Its .contents may change, but its .canvas and .addr
    may not. A Cell may not exist outside of a Canvas.'''
    addr: Addr  # must have a canvas specified
    contents: CellContents  # Node?

    def short(self) -> str:
        if self.contents is None:
            s = '(empty cell)'
        else:
            s = short(self.contents)
        return f'[ {s} ]'

    def set_contents(self, contents: CellContents) -> None:
        self.contents = contents

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({self.addr}: {short(self.contents)})'

@dataclass(frozen=True)
class Operator:
    func: Callable
    name: str
    min_num_operands: int = 2

    def __call__(self, *operands) -> int:  # HACK Numbo-specific return type
                                           # Likely fix: a type parameter
        if len(operands) < self.min_num_operands:
            raise NotEnoughOperands(
                actual_num_operands=len(operands),
                min_num_operands=self.min_num_operands
            )
        else:
            return self.func(*operands)

    def __hash__(self):
        '''It's necessary to omit self.func from __hash__ in order to maintain
        determinism. The default hash code of a function is its address in
        memory, which is non-deterministic. If necessary, one could include
        str(self.func).'''
        return hash(str(self))

    def mk_short(self, operands: Sequence[Value]) -> str:
        if operands:
            nm = f' {self.name} '
            return nm.join(short(o) for o in operands)
        else:
            return f'_{self.name}_'

plus = Operator(operator.add, '+')
mult = Operator(operator.mul, '*')

class Codelet(ABC):

    @abstractmethod
    def run(self, **kwargs) -> ProgramResult:
        pass

    def short(self) -> str:
        return as_dstr(self)

    def __str__(self) -> str:
        return short(self)    

@dataclass(frozen=True)
class Consume(Codelet):
    name: str
    operator: Operator
    operands: Tuple[Value, ...]

    def run(  # type: ignore[override]
        self,
        avails: Sequence[Value],
        operator: Operator,
        operands: Tuple[Value, ...]
    ) -> ProgramResult:
        operands, new_avails = take_avails(avails, operands)
        result = operator(*operands)
        return Produced(Avails(new_avails + (result,)))

    def short(self) -> str:
        return self.operator.mk_short(self.operands)

def take_avails(avails: Sequence[Value], to_take: Iterable[Value]) \
-> Tuple[Sequence[Value], Tuple[Value, ...]]:
    remaining_avails = list(avails)
    taken_avails: List[Value] = []
    missing_avails: List[Value] = []
    for v in to_take:
        try:
            remaining_avails.remove(v)
        except ValueError:
            taken_avails.append(None)
            missing_avails.append(v)
        else:
            taken_avails.append(v)
            missing_avails.append(None)
    if any(t is None for t in taken_avails):
        raise ValuesNotAvail(
            avails=tuple(taken_avails),
            unavails=tuple(missing_avails)
        )
    else:
        return (taken_avails, tuple(remaining_avails))

@dataclass(frozen=True)
class Produced:
    '''Something produced by a run of something, which probably should be
    deposited into the next cell of the current canvas, if appropriate.'''
    v: Value   # CellContents?

ProgramResult = Produced

class Fizzle(Exception):
    #codelet: Optional[Codelet] = None
    pass

@dataclass(frozen=True)
class ValuesNotAvail(Fizzle):
    avails: Tuple[Value, ...] = ()
        # These values were avail; indices match indices in seeker's request
    unavails: Tuple[Value, ...] = ()
        # These values were unavail; indices match indices in seeker's request

@dataclass(frozen=True)
class NotEnoughOperands(Fizzle):
    actual_num_operands: Optional[int] = None
    min_num_operands: Optional[int] = None

def Plus(*operands: int) -> Consume:
    return Consume(name='Plus', operator=plus, operands=as_tuple(operands))

WSElem = Union[Node, Canvas]

@dataclass
class FARGModelDataclassMixin:
    nodes: Set[Node] = field(default_factory=set)
    canvases: Set[Canvas] = field(default_factory=set)

class FARGModel(FARGModelDataclassMixin):

    def build(self, elem: Union[Value, Node, Canvas]) -> WSElem:
        if isinstance(elem, Canvas):
            self.canvases.add(elem)
            return elem
        else:
            raise NotImplementedError
        """
        else:
            node = as_node(elem)
        """

# Spike test

m = Avails(4, 5) + Plus(4, 5)
ps(m)
ca = ActionCanvas.make(Avails(4, 5), Plus(4, 5))
ps(ca)
fm = FARGModel()
b = fm.build(ca)
ps(b)
ca = fm.build(ActionCanvas.make(
    Avails(4, 5), Plus(4, 5)
))
ps(ca)
got = Plus(4, 5).run((4, 5), plus, (4, 5))
ps(got)
#NEXT: run
##fm.run(ca)
##ps(ca)
