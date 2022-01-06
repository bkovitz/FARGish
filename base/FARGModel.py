# FARGModel.py -- Base classes for a FARG model

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, get_type_hints
from abc import ABC, abstractmethod
import operator
import inspect

from Log import lo, trace
from util import ps, pr, as_tuple, short, as_dstr, as_dict, is_type_instance, \
    is_dataclass_instance, TypeAnnotation, singleton, first, as_iter, as_set

Value = Hashable
Values = Union[None, Hashable, Iterable[Hashable]]
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

class Codelet(ABC):

    @abstractmethod
    def run(self, **kwargs) -> ProgramResult:
        pass

    def short(self) -> str:
        return as_dstr(self)

    def __str__(self) -> str:
        return short(self)    

@dataclass(frozen=True)
class ArgsMap:
    '''A dictionary of argument names and their values, suitable for passing
    to a function.'''
    d: ArgsD

    def get(self, k: str, default: Optional[Value]=None) \
    -> Optional[Value]:
        if k == 'args':
            return self
        else:
            return self.d.get(k, default)

    def __add__(self, other) -> Union[ArgsMap, Node]:
        if isinstance(other, ArgsMap):
            return ArgsMap(other.d | self.d)
        elif isinstance(other, Node):
            return other.with_args_merged(self)
        else:
            return Node(other, self, None)

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

    def override_with(self, other: Args) -> Union[ArgsMap, None]:
        return self.merged(other, self)

    def as_argsmap(self) -> ArgsMap:
        return self

    def short(self) -> str:
        return ' '.join(f'{short(k)}={short(v)}' for k, v in self.d.items())

empty_args_map = ArgsMap({})

ArgsD = Dict[str, Value]
Args = Union[None, ArgsD, ArgsMap]

def as_argsmap(x: Any) -> ArgsMap:
    if hasattr(x, 'as_argsmap'):
        return x.as_argsmap()
    elif is_dataclass_instance(x) or isinstance(x, dict):
        return ArgsMap(as_dict(x))
    else:
        return empty_args_map

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

    # TODO rm; replace with calls to Canvas.jump(, NEXT)
    def next(self, cpart: Optional[str]=None) -> Addr:
        if cpart is None:
            return replace(self, index=self.index_as_int()+1)
        else:
            return replace(self, cpart=cpart, index=self.index_as_int()+1)

    def __str__(self) -> str:
        return as_dstr(self)

class Canvas(ABC):
    '''A Canvas is mutable. The contents of its cells may change, and the
    number of its cells may change.'''

    @abstractmethod
    def __getitem__(self, addr: Addr) -> CellContents:
        pass

    @abstractmethod
    def __setitem__(self, addr: Addr, v: Value) -> None:
        pass

    @abstractmethod
    def jump(self, addr: Addr, relation: Values) -> Set[Addr]:
        '''Starting at 'addr', what Addr or Addrs lie 'relation' away?

        Might throw RelationUnknownToCanvas.'''
        pass

    def jump_or_fizzle(self, addr: Addr, relation: Value) -> Addr:
        '''Same as .jump(), but returns a single Addr or raises Not1AddrThere
        if there is not exactly one Addr in the Canvas 'relation' away
        from 'addr'.'''
        result = self.jump(addr, relation)
        if len(result) != 1:
            raise Not1AddrThere(canvas=self, addr=addr, relation=relation)
        else:
            return first(result)

@singleton
class Relation:
    
    def __init__(self, name: str):
        self.name = name

NEXT = Relation('NEXT')
PREV = Relation('PREV')

SITUATION = Relation('SITUATION')
ACTION = Relation('ACTION')

@dataclass
class ActionCanvas(Canvas, Codelet):
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

    def jump(self, addr: Addr, relation: Values) -> Set[Addr]:
        addrs = as_set(addr)
        for r in as_iter(relation):
            new_addrs = set()
            for addr in addrs:
                new_addrs |= self._single_jump(addr, r)
            addrs = new_addrs
        return addrs

    def _single_jump(self, addr: Addr, relation: Value) -> Set[Addr]:
        if relation is NEXT:
            return self._jump_delta(addr, +1)
        elif relation is PREV:
            return self._jump_delta(addr, -1)
        elif relation is SITUATION:
            return {replace(addr, cpart='situation')}
        elif relation is ACTION:
            return {replace(addr, cpart='action')}
        else:
            raise RelationUnknownToCanvas(
                canvas=self, addr=addr, relation=relation
            )

    def _jump_delta(self, addr: Addr, delta: int) -> Set[Addr]:
        old_i = addr.index_as_int()
        new_i = old_i + delta
        if new_i < 0:
            return set()
        else:
            return {replace(addr, index=new_i)}

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

    def paint(self, addr: Addr, v: Value) -> None:
        self[addr] = v
        # TODO Return the old value? Put the old value in the soup?

    def run(self, args: Optional[ArgsMap]=None) -> ProgramResult:  # type: ignore[override]
        if not self._action_cells:
            return Produced(None)
        if args is None:
            args = empty_args_map
        for action_cell in self._action_cells:
            # TODO prepend args from situation cell
            current_situation = self[action_cell.new_addr(cpart='situation')]
            if isinstance(current_situation, ArgsMap):
                args = ArgsMap.merged(current_situation, args)
            result = action_cell.run(args)
            #self.paint(action_cell.addr.next(cpart='situation'), result.v)
            self.paint(
                self.jump_or_fizzle(action_cell.addr, (SITUATION, NEXT)),
                result.v
            )
            # TODO tag SuccessfulToHere
            # update args
        return Produced(result.v)

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

    def new_addr(self, **kwargs) -> Addr:
        return replace(self.addr, **kwargs)

    def run(self, args: Optional[ArgsMap]=None) -> ProgramResult:
        if self.contents is None:
            raise NotImplementedError  # TODO RunAborted(self.addr)
        elif isinstance(self.contents, Codelet):
            return FARGModel.xrun(self.contents, args)
        else:
            return Produced(None)

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

@dataclass(frozen=True)
class Consume(Codelet):
    name: str
    operator: Operator
    operands: Tuple[Value, ...]

    def run(  # type: ignore[override]
        self,
        avails: Tuple[Value, ...],
        operator: Operator,
        operands: Tuple[Value, ...]
    ) -> ProgramResult:
        operands, new_avails = take_avails(avails, operands)
        result = operator(*operands)
        return Produced(Avails(*(new_avails + (result,))))

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

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.v)})'

ProgramResult = Produced

class Fizzle(Exception):
    #codelet: Optional[Codelet] = None

    def __str__(self) -> str:
        return as_dstr(self)

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

@dataclass(frozen=True)
class RelationUnknownToCanvas(Fizzle):
    '''Something tried to .jump on a Canvas with a relation that is not
    defined for the Canvas.'''
    canvas: Optional[Canvas] = None
    addr: Optional[Addr] = None  # caller tried to jump from this Addr
    relation: Optional[Value] = None

@dataclass(frozen=True)
class Not1AddrThere(Fizzle):
    '''Something tried to .jump from an Addr via a relation that led to
    either no Addr (no place that could be on the Canvas) or more than
    one Addr (an ambiguous jump).'''
    canvas: Optional[Canvas] = None
    addr: Optional[Addr] = None  # caller tried to jump from this Addr
    relation: Optional[Value] = None

def Plus(*operands: int) -> Consume:
    return Consume(name='Plus', operator=plus, operands=as_tuple(operands))

WSElem = Union[Node, Canvas]

@dataclass
class FARGModelDataclassMixin:
    nodes: Set[Node] = field(default_factory=set)
    canvases: Set[Canvas] = field(default_factory=set)

class FARGModel(FARGModelDataclassMixin):

    B = TypeVar('B', bound=Union[Value, Node, Canvas])
    def build(self, elem: B) -> B:
        if isinstance(elem, Canvas):
            self.canvases.add(elem)
            return elem  # type: ignore[return-value]
        else:
            raise NotImplementedError
        """
        else:
            node = as_node(elem)
        """

    def run(self, codelet: Codelet, args: Args=empty_args_map) \
    -> ProgramResult:
        return self.xrun(codelet, ArgsMap.merged(dict(fm=self), args))

    @classmethod
    def xrun(cls, codelet: Codelet, args: Args) -> ProgramResult:
        '''Generic 'run' method without necessarily specifying a FARGModel
        object.'''
        try:
            return codelet.run(
                **cls.mk_func_args(codelet.run, ArgsMap.merged(
                    args,
                    as_argsmap(codelet)
                ))
            )
        except Fizzle as fiz:
            # TODO
            raise

    @classmethod
    def mk_func_args(cls, func: Callable, args: Args) -> Dict[str, Any]:
        d: Dict[str, Any] = {}
        args = as_argsmap(args)
        for param_name, param_type in cls.params_of(func):
            value = args.get(param_name, None)
            if not is_type_instance(value, param_type):
                # TODO Distinguish between missing argument and argument of
                # wrong type?
                lo('WRONGTYPE', param_name, param_type, value)
                print('???')
                lo('ARGS', type(args), short(args))
                raise NotImplementedError  # TODO MissingArgument
            else:
                d[param_name] = value
        return d

    @classmethod
    def params_of(cls, func: Callable) -> Iterable[Tuple[str, TypeAnnotation]]:
        type_hints = get_type_hints(func, globalns=func.__globals__) # type: ignore[attr-defined]
        for param_name in inspect.signature(func).parameters:
            if param_name == 'return':
                continue  # disregard return type
            yield (param_name, type_hints.get(param_name, Any))

    def short(self) -> str:
        return self.__class__.__name__

# Spike test
if __name__ == '__main__':
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
    got = ca.run()
    ps(got)
    got = fm.run(ca)
    ps(got)
    ps(ca)
