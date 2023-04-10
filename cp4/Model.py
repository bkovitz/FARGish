# Model.py -- The canvas-and-painters model

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check, get_type_hints, get_args
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod
from itertools import chain
from collections import defaultdict
import re
from pprint import pp

from pyrsistent import pmap
from pyrsistent.typing import PMap

from Log import lo, trace
from util import as_iter, field_names_and_values, first, force_setattr, \
    intersection, pr, safe_issubclass, short, union


T = TypeVar('T')
ARG = TypeVar('ARG', bound='Argument')

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
            case CompoundWorkspaceObj():
                return v.with_var_level(level)  # type: ignore[return-value]
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

VDict = Dict[Variable, Parameter]

Letter = str  # strictly speaking, a single-character, lowercase str
Index = int

@dataclass(frozen=True)
class CompoundWorkspaceObj(ABC):

    def params(self) -> Collection[Variable]:
        result: List[Variable] = []
        for name, typ in get_type_hints(self.__class__).items():
            if is_parameter(typ):
                value = getattr(self, name)
                if is_variable(value):
                    result.append(value)
        return result

    def with_var_level(self, level: int) -> CompoundWorkspaceObj:
        '''Return a new CompoundWorkspaceObj, in which all the Parameter
        variables have been replaced with Var objects at 'level'.'''
        return self.__class__(*(  # type: ignore[arg-type]
            Var.at_level(arg, level)
                for arg in all_arguments_of(self)
        ))

    @abstractmethod
    def eval(self: T, ws: Workspace) -> T:
        pass

    def replace_constants_with_variables(self, ws: Workspace) \
    -> CompoundWorkspaceObj:
        return self.__class__(*(
            self.replace_constant_with_variable(ws, arg) #type: ignore[arg-type]
                for arg in all_arguments_of(self)
        ))
        # TODO: optimization? If all the args are variables or None alrady,
        # then return self instead of creating a new object

    def replace_constant_with_variable(self, ws: Workspace, arg: Argument) \
    -> Union[Variable, None]:
        if is_variable(arg):
            return arg
        elif arg is None:
            return None
        else:
            return ws.define_and_name(arg) # type: ignore[arg-type] #mypy bug

########## Tags ##########

class Tag:
    pass

Tags = Union[
    Tag,
    Tuple[Union[Tag, None], ...],
    Set[Tag],
    List[Union[Tag, None]],
    None
]

@dataclass(frozen=True)
class SideTag(Tag, ABC):
    @abstractmethod
    def opposite(self) -> SideTag:
        pass

    @classmethod
    def is_opposite_side(cls, tag1: Optional[Tag], tag2: Optional[Tag]) -> bool:
        match (tag1, tag2):
            case (Lhs(), Rhs()):
                return True
            case (Rhs(), Lhs()):
                return True
            case _:
                return False

@dataclass(frozen=True)
class Lhs(SideTag):
    def opposite(self) -> SideTag:
        return Rhs()

@dataclass(frozen=True)
class Rhs(SideTag):
    def opposite(self) -> SideTag:
        return Lhs()

@dataclass(frozen=True)
class WorldTag(Tag):
    pass

@dataclass(frozen=True)
class OldWorld(WorldTag):
    pass

@dataclass(frozen=True)
class NewWorld(WorldTag):
    pass

def extract_tag(tagtype: Type[Tag], tags: Tags) -> Optional[Tag]:
    '''Searches through 'tags' and returns the first tag of 'tagtype', or
    None if not found.'''
    for tag in as_iter(tags):
        if isinstance(tag, tagtype):
            return tag
    return None

########## Subst ##########

def argtype(a: Argument) -> Literal['BaseObj', 'CompoundWorkspaceObj', 'Variable']:
    if is_variable(a):
        return 'Variable'
    elif isinstance(a, CompoundWorkspaceObj):
        return 'CompoundWorkspaceObj'
    else:
        return 'BaseObj'

@dataclass(frozen=True)
class Subst:
    d: PMap[Argument, Argument] = field(default_factory=pmap)
    # TODO Change to PMap[Variable, Argument]

    @classmethod
    def from_kwargs(cls, **kwargs) -> Subst:
        result = empty_subst
        for k, v in kwargs.items():
            result = result.unify(k, v)
        return result

    def __contains__(self, a: Variable) -> bool:
        return a in self.d

    def __or__(self, other: Subst) -> Subst:
        return Subst(self.d.update(other.d))
        
    def items(self) -> Iterable[Tuple[Argument, Argument]]:
        return self.d.items()

    def values(self) -> Iterable[Argument]:
        return self.d.values()

    def get(self, k: Argument, default: Any=None) -> Any:
        try:
            return self.d[k]
        except KeyError:
            return default

    def at_level(self, level: int) -> Subst:
        result = Subst()
        for lhs, rhs in self.items():
            result = result.unify(
                Var.at_level(lhs, level),
                rhs
            )
        return result
        
    def unify(self, a1: Argument, a2: Argument) -> Subst:
        if a1 == a2:
            return self
        match (argtype(a1), argtype(a2)):
            case ('BaseObj', 'BaseObj'):
                return BottomSubst()  # we know that a1 != a2
            case ('BaseObj', 'Variable'):
                return self.unify(a2, a1)
            case ('BaseObj', 'CompoundWorkspaceObj'):
                return bottom_subst
            case ('Variable', 'BaseObj'):
                if a1 in self.d:
                    return self.unify(self.d[a1], a2)
                else:
                    return Subst(self.d.set(a1, a2))
            case ('Variable', 'Variable'):
                if a1 in self.d:
                    if a2 in self.d:
                        return self.unify(self.d[a1], self.d[a2])
                    else:
                        return self.unify(a2, a1)
                else:
                    return Subst(self.d.set(a1, a2))
            case ('Variable', 'CompoundWorkspaceObj'):
                if a1 in self.d:
                    return self.unify(self.d[a1], a2)
                else:
                    if self.occurs_in(a1, a2):  # type: ignore[arg-type]
                        return bottom_subst
                    else:
                        return Subst(self.d.set(a1, a2))
            case ('CompoundWorkspaceObj', 'BaseObj'):
                return bottom_subst
            case ('CompoundWorkspaceObj', 'Variable'):
                return self.unify(a2, a1)
            case ('CompoundWorkspaceObj', 'CompoundWorkspaceObj'):
                result = self
                for v1, v2 in zip(
                        all_arguments_of(a1),  #type: ignore[assignment, arg-type]
                        all_arguments_of(a2)   #type: ignore[assignment, arg-type]
                    ):
                    result = result.unify(v1, v2)
                return result
            case _:
                raise NotImplementedError(a1, a2)

    def is_bottom(self) -> bool:
        return False

    def remove(self, var: Variable) -> Subst:
        # return Subst(self.d.discard(var))
        val = self.d.get(var)
        if val is None:
            return self
        else:
            d = self.d.discard(var)
            return Subst(d).replace_all(var, val)

    @classmethod
    def occurs_in(cls, var: Variable, a: Argument) -> bool:
        for v in cls.vars(a):
            if var == v:
                return True
        return False

    @classmethod
    def vars(cls, a: Argument) -> Iterable[Variable]:
        match argtype(a):
            case 'BaseObj':
                return
            case 'Variable':
                yield a  # type: ignore[misc]
            case 'CompoundWorkspaceObj':
                yield from chain.from_iterable(
                    cls.vars(arg)
                        for arg in all_arguments_of(a)  # type: ignore[arg-type]
                )

    def eval(self, a: Argument) -> Optional[WorkspaceObj]:
        match a:
            case _ if is_variable(a):
                match (v := self.d.get(a)):
                    case None:
                        return None
                    case _:
                        return self.eval(v) # type: ignore[arg-type]  #mypy bug
            case str():
                return a
            case int():
                return a
            case CompoundWorkspaceObj():
                return a.__class__(
                    *(self.eval(arg) for arg in all_arguments_of(a))
                )
        raise NotImplementedError

    # TODO rm?
    def are_equal(self, a1: Argument, a2: Argument) -> bool:
        match a1:
            case _ if is_variable(a1):
                if a1 not in self.d:
                    return False
                #return self.d.get(a1) == self.d.get(a2)
        return True

    def replace_all(self, lhs: Argument, rhs: Argument) -> Subst:
        '''Returns a Subst in which every occurence of 'lhs' has been replaced
        by 'rhs'.'''
        result = empty_subst
        for l, r in self.d.items():
            result = result.unify(
                #self.substitute_in(l, lhs, rhs),
                l,
                self.substitute_in(r, lhs, rhs)
            )
            if not result:
                return bottom_subst
        return result

    @classmethod
    def substitute_in(cls, a: Argument, lhs: Argument, rhs: Argument) -> Argument:
        '''Returns a new Argument consisting of a where every occurrence of lhs
        has been replaced by rhs. Does not affect PainterClusters.'''
        match a:
            case x if x == lhs:
                return rhs
            case PainterCluster():
                return a
            case CompoundWorkspaceObj():
                return a.__class__(
                    *(cls.substitute_in(arg, lhs, rhs)
                        for arg in all_arguments_of(a)
                    )
                )
            case _:  # no match; nothing to substitute
                return a
        raise NotImplementedError  # Should never get here: mypy bug

    def pr(self) -> None:
        for k, v in sorted(self.items(), key=str):
            print(f'{k}: {short(v, inside=True)}')
        print()

    def __str__(self) -> str:
        cl = self.__class__.__name__
        dstr = ', '.join(f'{k}={v!r}' for k, v in self.d.items())
        return f'{cl}({dstr})'

    short = __str__

class BottomSubst(Subst):

    def unify(self, a1: Argument, a2: Argument) -> Subst:
        return self

    def eval(self, a: Argument) -> Optional[WorkspaceObj]:
        return None

    def is_bottom(self) -> bool:
        return True

empty_subst = Subst()
bottom_subst = BottomSubst()

########## Fizzles ##########

class Fizzle(Exception):
    pass

@dataclass(frozen=True)
class WrongType(Fizzle):
    typ: Any  # What the type was supposed to be
    ref: Any  # The reference to the object of the wrong type
    obj: Any  # The object with the wrong type

@dataclass(frozen=True)
class CantRun(Fizzle):
    '''Indicates that a Painter couldn't run.'''
    pass

class ArgumentsFailRelation(Fizzle):
    '''Indicates that both of a Painter's arguments exist but are in a relation
    that conflicts with that stored in the Painter, e.g. LengthPainter(Same)
    where the Canvases have different lengths.'''
    pass

########## The canvas ##########

@dataclass
class Canvas:
    contents: str
    length: Optional[int]

    def __hash__(self):
        return hash(id(self))

    def __eq__(self, other):
        return self is other

    @classmethod
    def make_from(cls, s: str) -> Canvas:
        return Canvas(contents=s, length=len(s))

    @classmethod
    def parse_analogy_string(cls, s: str) -> List[Canvas]:
        #old_world, new_world = s.split(";")
        m = re.match('([a-z]+)\s*->\s*([a-z]+)\s*;\s*([a-z]+)\s*->\s*\?', s)
        if m is not None:
            return [
                cls.make_from(group) for group in m.groups()
            ] + [cls.make_unknown()]
        else:
            return []
    
    @classmethod
    def are_identical(cls, c1: Canvas, c2: Canvas) -> bool:
        return (
            c1.contents == c2.contents
            and
            c1.length == c2.length
        )

    @classmethod
    def make_unknown(cls, length: Optional[int]=None) -> Canvas:
        return Canvas(contents='', length=length)

    # TODO UT
    def __getitem__(self, i: Index) -> Optional[str]:
        try:
            return self.contents[i - 1]
        except IndexError:
            return None

    def all_indices(self) -> Iterable[Index]:
        if self.length is None:
            return ()
        else:
            return range(1, self.length + 1)

    def replace_contents(self, s: str) -> None:
        self.contents = s

    def addr(self, i: Index) -> CanvasAddress:
        return CanvasAddress(self, i)

    def __str__(self) -> str:
        return self.contents

    def short(self) -> str:
        return 'c' + repr(self.contents)

@dataclass(frozen=True)
class Address(CompoundWorkspaceObj):
    pass

@dataclass(frozen=True)
class CanvasAddress(Address):
    canvas: Parameter[Canvas]
    i: Parameter[Index]

    def eval(self, ws: Workspace) -> CanvasAddress:
        return CanvasAddress(
            ws.get_canvas(self.canvas),
            ws.get_index(self.i)
        )

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.canvas)}, {self.i})'

    __repr__ = __str__

@dataclass(frozen=True)
class ParameterAddress(Address):
    painter: Parameter[Painter]
    parameter_name: Parameter[ParameterName]  # Can't be indirect, i.e.
                                        # must be a variable, not a constant

    def eval(self, ws: Workspace) -> ParameterAddress:
        return ParameterAddress(
            ws.get_painter(self.painter),
            self.parameter_name
        )


class Op(ABC):

    @classmethod
    @abstractmethod
    def has_relation(cls, x1: Any, x2: Any) -> bool:
        pass

    # TODO UT  Fail when reaching 'z' or 'a'
    @classmethod
    def make(
        cls,
        start_letter: str,
        length: int,
        start_index: int=1,
        exception: Optional[Exception_]=None
    ) -> str:
        return (
            cls.reverse_sequence(start_letter, 1, start_index, exception)
            +
            cls.sequence(start_letter, start_index, length, exception)
        )

    # TODO Rewrite .make() top-down

    @classmethod
    def reverse_sequence(
        cls,
        seed_letter: str,
        from_index: Index,
        end_index_exclusive: Index,
        exception: Optional[Exception_]=None
    ) -> str:
        result: List[str] = []
        for i in range(from_index, end_index_exclusive):
            actual_letter, seed_letter = \
                cls.generate_prev(seed_letter, i, exception)
            result.append(actual_letter)
        result.reverse()
        return ''.join(result)

    @classmethod
    def sequence(
        cls,
        seed_letter: str,
        from_index: Index,
        to_index: Index,
        exception: Optional[Exception_]=None
    ) -> str:
        result: List[str] = []
        for i in range(from_index, to_index + 1):
            if i != from_index:
                #start_letter = cls.next_letter(start_letter)
                actual_letter, seed_letter = \
                    cls.generate_next(seed_letter, i, exception)
            else:
                actual_letter = seed_letter
            result.append(actual_letter)
        return ''.join(result)

    @classmethod
    def generate_next(
        cls,
        seed_letter: str,
        i: Index,
        exception: Optional[Exception_]=None
    ) -> Tuple[str, str]:
        '''Returns (actual_letter, seed_letter) where seed_letter is the
        letter to pass generate_next() next time. An exception can make
        actual_letter different from seed_letter.'''
        match exception:
            case None:
                actual_letter = cls.next_letter(seed_letter)
                return actual_letter, actual_letter
            case Skip(skip_i):
                if i == skip_i:
                    seed_letter = cls.next_letter(seed_letter)
                    seed_letter = cls.next_letter(seed_letter)
                    return seed_letter, seed_letter
                else:
                    return cls.generate_next(seed_letter, i, exception=None)
            case _:
                raise NotImplementedError

    @classmethod
    def generate_prev(
        cls,
        seed_letter: str,
        i: Index,
        exception: Optional[Exception_]=None
    ) -> Tuple[str, str]:
        match exception:
            case None:
                actual_letter = cls.prev_letter(seed_letter)
                return actual_letter, actual_letter
            case Skip(skip_i):
                if i == skip_i:
                    seed_letter = cls.prev_letter(seed_letter)
                    seed_letter = cls.prev_letter(seed_letter)
                    return seed_letter, seed_letter
                else:
                    return cls.generate_prev(seed_letter, i)
            case _:
                raise NotImplementedError

    @classmethod
    @abstractmethod
    def next_letter(cls, letter: str) -> str:
        pass

    @classmethod
    @abstractmethod
    def prev_letter(cls, letter: str) -> str:
        pass

@dataclass(frozen=True)
class Succ(Op, CompoundWorkspaceObj):
    left: Parameter[Address]
    right: Parameter[Address]

    @classmethod
    def examine_pair(cls, ws: Workspace, a1: Address, a2: Address) \
    -> Iterable[Succ]:
        '''If the element at a2 is the successor of a1, yields Succ(a1, a2).
        Otherwise yields nothing.'''
        match (ws.at(a1), ws.at(a2)):
            case (str(l), str(r)):
                if cls.is_succ(l, r):
                    yield Succ(a1, a2)
            case (CanvasAddress(c1_, i1_),
                  CanvasAddress(c2_, i2_)):
                c1 = ws.get_canvas(c1_)
                c2 = ws.get_canvas(c2_)
                i1 = ws.get_index(i1_)
                i2 = ws.get_index(i2_)
                if c1 is c2 and i2 == i1 + 1:
                    yield Succ(a1, a2)

#                    pcm = PCMaker()
#                    #aa1 = pcm.define_address(a1)
#                    #aa2 = pcm.define_address(a2)
#                    pp1 = pcm.define_painter(Succ(aa1, aa2))
#                    pcm.add_relationship(Succ, aa1, aa2)
#
#                    # Pass constants & relationships to pcm.
#                    # pcm doesn't detect relationships except 'same'.
#                    # pcm invents all variables.
#                    # Pass 'what to build' to pcm.
#
#                    pcm.rebuild_objects_with_args(a1, a2)
#                    pcm.add_relationship(Succ, i1, i2)
#                    yield pcm.painter_cluster()


    def eval(self, ws: Workspace) -> Succ:
        return Succ(
            ws.get_address(self.left), 
            ws.get_address(self.right)
        )

    @classmethod
    def is_succ(cls, l1: str, l2: str) -> bool:
        return ord(l2) == ord(l1) + 1

    @classmethod
    def has_relation(cls, x1: Any, x2: Any) -> bool:
        match (x1, x2):
            case (str(), str()):
                if x1 >= 'z':
                    return False
                return ord(x1) + 1 == ord(x2)
        return False

    # TODO what about 'z'?
    @classmethod
    def next_letter(cls, letter: str) -> str:
        return chr(ord(letter) + 1)

    # TODO what about 'a'?
    @classmethod
    def prev_letter(cls, letter: str) -> str:
        return chr(ord(letter) - 1)

class Pred(Op):

    @classmethod
    def has_relation(cls, x1: Any, x2: Any) -> bool:
        match (x1, x2):
            case (str(), str()):
                if x1 <= 'a':
                    return False
                return ord(x1) - 1 == ord(x2)
        return False

    # TODO UT  Fail when reaching 'a'
    @classmethod
    def next_letter(cls, letter: str) -> str:
        return chr(ord(letter) - 1)

    # TODO what about 'z'?
    @classmethod
    def prev_letter(cls, letter: str) -> str:
        return chr(ord(letter) + 1)

@dataclass(frozen=True)
class Same(Op):

    @classmethod
    def has_relation(cls, x1: Any, x2: Any) -> bool:
        return x1 == x2

    @classmethod
    def next_letter(cls, letter: str) -> str:
        return letter

    @classmethod
    def prev_letter(cls, letter: str) -> str:
        return letter

class Exception_(CompoundWorkspaceObj):
    pass

@dataclass(frozen=True)
class Skip(Exception_):
    i: Parameter[Index]

    def eval(self, ws: Workspace) -> Skip:
        i = ws.eval(self.i)
        if isinstance(i, int):
            return Skip(i)
        else:
            raise NotImplementedError  # TODO

#    def replace_constants_with_variables(self, ws: Workspace) \
#    -> CompoundWorkspaceObj:
#        if is_variable(self.i):
#            return self
#        else:
#            return Skip(ws.define_and_name(self.i))  # type: ignore[arg-type]
    
@dataclass(frozen=True)
class Succeeded:
    info: Any

ops: Iterable[Type[Op]] = (Same, Succ, Pred)

def detect_repetition(canvas: Canvas) -> Optional[Repeat]:
    if canvas.length is None:
        return None
    start_letter = canvas[1]
    if start_letter is not None:
        for op in ops:
            perfect = op.make(start_letter, canvas.length)
            match op_to_repeater(op, canvas, perfect):
                case Succeeded(flaw):
                    return Repeat(
                        canvas,
                        Seed(start_letter, 1),
                        op,
                        exception=flaw
                    )
    return None

def op_to_repeater(
    op: Type[Op],
    canvas: Canvas,
    perfect: str
) -> Optional[Succeeded]:
    flaw: Optional[Skip] = None
    j = 0  # index into 'perfect'
    for i in canvas.all_indices():
        if canvas[i] == perfect[j]:
            pass
        elif canvas[i] == op.next_letter(perfect[j]):
            if flaw is not None:
                return None   # 2nd flaw, so give up
            else:
                flaw = Skip(i)
                j += 1
        else:
            return None
        j += 1
    return Succeeded(flaw)

@dataclass(frozen=True)
class Seed(CompoundWorkspaceObj):
    letter: Parameter[Letter]
    i: Parameter[Index]

    def get_i(self, ws: Workspace) -> Index:
        return ws.get_index(self.i)

    def get_letter(self, ws: Workspace) -> str:
        return ws.get_letter(self.letter)

    def eval(self, ws: Workspace) -> Seed:
        return Seed(
            #ws.eval(self.letter),
            #ws.eval(self.i)
            ws.get_letter(self.letter),
            ws.get_index(self.i)
        )

    def __repr__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({self.letter!r}, {self.i})'

#class DeterminateSeed:
#    letter: str
#    i: Index
    

@dataclass(frozen=True)
class Repeat(CompoundWorkspaceObj):
    canvas: Parameter[Canvas]
    seed: Parameter[Seed]
    op: Parameter[Type[Op]]
    exception: OptionalParameter[Exception_] = None

    def fill(self, ws: Workspace) -> None:
        canvas = ws.get_canvas(self.canvas)
        seed = ws.get_seed(self.seed)
        op = ws.get_op(self.op)
        exception = ws.get_exception(self.exception)

        if canvas.length is not None:
            canvas.replace_contents(
                op.make(
                    seed.get_letter(ws),
                    canvas.length,
                    seed.get_i(ws),
                    exception
                )
            )
        # TODO supply canvas length if .length is None

    def eval(self, ws: Workspace) -> Repeat:
        return Repeat(
            ws.get_canvas(self.canvas),
            ws.get_seed(self.seed),
            ws.get_op(self.op),
            ws.get_exception(self.exception)
        )

@dataclass(frozen=True)
class OtherSide(CompoundWorkspaceObj):
    left: Variable     # TODO allow a constant?
    right: Variable    # TODO allow a constant?

    def run(self, ws: Workspace) -> None:
        match (ws.eval(self.left), ws.eval(self.right)):
            case (Canvas(), None):
                #TODO Let mate be the variable holding the object, not the
                # object itself.
                mate = ws.variable_of(ws.find_object_with_tag(
                    self.other_side_tags(ws, self.left)
                ))
                if mate is not None:
                    ws.unify(self.right, mate)
                else:
                    raise NotImplementedError
                    # TODO what if mate does not exist?
            case (None, Canvas()):
                mate = ws.variable_of(ws.find_object_with_tag(
                    self.other_side_tags(ws, self.right)
                ))
                if mate is not None:
                    ws.define(self.left, mate)
                else:
                    raise NotImplementedError
                    # TODO what if mate does not exist?
            case (None, None):
                raise CantRun
            case _:
                raise NotImplementedError(
                    (self.left, self.right),
                    (ws.eval(self.left), ws.eval(self.right))
                )
                # TODO the failure cases
                
    def other_side_tags(self, ws: Workspace, obj: Variable) -> Tags:
        '''Returns tags to specify an object on the other "side" (Lhs/Rhs) and
        the same "world".'''
        tags = ws.tags_of(obj)
        match (extract_tag(SideTag, tags), extract_tag(WorldTag, tags)):
            case (SideTag() as side_tag, WorldTag() as world_tag):
                return (side_tag.opposite(), world_tag)
            case (SideTag() as side_tag, None):
                raise NotImplementedError  # TODO
                return side_tag.opposite()
        return None  # TODO other cases

    def eval(self, ws: Workspace) -> OtherSide:
        raise NotImplementedError  # TODO

    def replace_constants_with_variables(self, ws: Workspace) -> CompoundWorkspaceObj:
        return self

#    def complete(self, ws: Workspace) -> Completion:
#        pass
#        match (ws[self.left], ws[self.right]):
#            case (None, None):
#                return NoCompletion()
#            case (Canvas(), None):
#                pass # TODO
#            case (None, Canvas()):
#                pass # TODO
#            case (Canvas(), Canvas()):
#                pass # TODO

@dataclass(frozen=True)
class LengthPainter(CompoundWorkspaceObj):
    left: Parameter[Canvas]
    right: Parameter[Canvas]
    relation: Parameter[Painter]  # TODO More-specific type?

    def run(self, ws: Workspace) -> None:
        right = ws.get_canvas(self.right)
        left = ws.get_canvas(self.left)
        #right.length = 3
        if self.relation != Same:
            raise NotImplementedError
        # TODO fix HACK: this code implements the Same relation; self.relation
        # should specify the relation.
        match (left.length, right.length):
            case (int(), None):
                right.length = left.length
            case (None, int()):
                left.length = right.length
            case (int(), int()):
                if left.length != right.length:
                    raise ArgumentsFailRelation

    def eval(self, ws: Workspace) -> LengthPainter:
        return LengthPainter(
            ws.get_canvas(self.left),
            ws.get_canvas(self.right),
            ws.get_painter(self.relation),
        )

class ArgumentRelationDetector:

#    def examine_painter(self, ws: Workspace, p: Painter) \
#    -> Iterable[Painter]:
    @classmethod
    def examine_pair(
        cls, ws: Workspace, a1: ParameterAddress, a2: ParameterAddress
    ) -> Iterable[Painter]:
        return Succ.examine_pair(ws, a1, a2)
#        match (a1, a2):
#            case (ParameterAddress(p1, i1), ParameterAddress(p2, i2)):
#                re
#                relation_painter = list(Succ.examine_pair(a1, a2))[0]
#
#        return [PainterCluster(
#                Define('PP1', Succ('AA1', 'AA2')),
#                Define('AA1', CanvasAddress('SS', 'II1')),
#                Define('AA2', CanvasAddress('SS', 'II2')),
#                Succ('II1', 'II2')
#            )]

@dataclass(frozen=True)
class Define(CompoundWorkspaceObj):
    '''Contrary to the name of what Define inherits from, a Define may not
    exist as a full citizen of the Workspace. It may only be an element of a
    PainterCluster.'''
    name: Variable
    value: Any

    def with_variables_at_level(self) -> Define:
        raise NotImplementedError

    def params(self) -> List[Variable]:
        result: List[Variable] = [self.name]
        if is_variable(self.value):
            result.append(self.value)
        else:
            if isinstance(self.value, CompoundWorkspaceObj):
                result += self.value.params()
        return result

    def with_var_level(self, level: int) -> Define:
        return Define(
            Var.at_level(self.name, level),
            Var.at_level(self.value, level)  #self.value.with_var_level(level)
        )

    def run_local(self, local_ws: Workspace) -> None:
        #lo('RLO', self.name, self.name in local_ws.subst, local_ws.subst)
        local_ws.define(self.name, self.value)
        
    def eval(self, ws: Workspace) -> Define:
        raise NotImplementedError  # TODO

    def replace_constants_with_variables(self, ws: Workspace) -> CompoundWorkspaceObj:
        return self  # DOUBT Is this right? Do we need to replace constants
                     # inside self.value?
    def short(self) -> str:
        return f'{self.name}={short(self.value)}'

# TODO rm?
def is_level_0(x: Argument) -> bool:
    match x:
        case Var(level=level):
            return level == 0
        case str():     # letter or level-0 variable
            return True
        case int():
            return True
        case Canvas():
            return True
        case _ if is_type_op(x):
            return True
        case CompoundWorkspaceObj():
            return all(is_level_0(arg) for arg in all_arguments_of(x))
    raise NotImplementedError  # mypy bug: it thinks x is Type[Op]

# TODO rm?
def try_to_make_level_0(subst: VDict, o: CompoundWorkspaceObj) \
-> Optional[CompoundWorkspaceObj]:
    d: Dict[str, Any] = {}
    for name, v in parameters_and_arguments_of(o):
        if is_base_obj(v):
            continue
        elif is_variable(v):    # TODO what if v is level-0?
            if is_level_0(v):
                continue
            else:
                # TODO If vv is not a variable, give up
                vv = subst.get(v, None)
                if is_variable(vv) and is_level_0(vv):
                    d[name] = vv
                else:
                    return None
        elif isinstance(v, CompoundWorkspaceObj):
            raise NotImplementedError
        else:
            raise NotImplementedError
    return replace(o, **d)

def is_parameter(typ: Collection) -> bool:
    '''Is typ a Parameter, OptionalParameter, or Value?'''
    return Var in get_args(typ)

def parameters_and_arguments_of(o: CompoundWorkspaceObj) \
-> Iterable[Tuple[str, Argument]]:
    for name, typ in get_type_hints(o.__class__).items():
        if is_parameter(typ):
            value = getattr(o, name)
            #if value is not None:
            yield (name, value)
    
def all_arguments_of(o: CompoundWorkspaceObj) -> Iterable[Argument]:
    for name, typ in get_type_hints(o.__class__).items():
        if is_parameter(typ):
            value = getattr(o, name)
            yield value        # *all* arguments of o?

def single_letter_name_for(o: WorkspaceObj) -> str:
    match o:
        case str():  # Letter
            return 'L'
        case Seed():
            return 'D'
        case int():
            return 'I'
        case _ if is_type_op(o):
            return 'F'
        case Repeat():
            return 'R'
        case Exception_():
            return 'E'
        case Canvas():
            return 'S'  # "snippet"
        case PainterCluster():
            return 'U'
        case Succ():
            return 'P'  # "painter"
        case Address():
            return 'A'
        case _:
            raise NotImplementedError(o)
    raise NotImplementedError

@dataclass(frozen=True)
class PainterCluster(CompoundWorkspaceObj):
    elems: Tuple[PainterClusterElem, ...]

    def __init__(self, *elems: CompoundWorkspaceObj):
        force_setattr(self, 'elems', elems)

    def __eq__(self, other: Any) -> bool:
        return (
            isinstance(other, PainterCluster)
            and
            set(self.elems) == set(other.elems)
        )

    def params(self) -> Set[Variable]:
        return union(*(elem.params() for elem in self.elems))

    def with_var_level(self, level: int) -> PainterCluster:
        return self # TODO Is this right? Or should we make a new PainterCluster
                    # with new elems?

    def run(self, ws_in: Workspace, subst_in: Subst) -> Subst:  #VDict:
        '''Returns new Subst, to put into ws_in.'''
        # Set up local variables
        local_ws = replace(ws_in, subst=ws_in.subst)
        for k, v in subst_in.items():
            local_ws.define(Var.at_level(k, 1), v)  # type: ignore[arg-type]
                            # TODO Subst.d should be PMap[Variable, Argument]

        # Run the elements
        elems = list(self.elems_at_level(1))  # TODO convert to a set?
        while elems:
            # TODO Need to catch infinite loop if at least one elem was not
            # removed on previous iteration.
            for elem in list(elems):
                try:
                    if isinstance(elem, Define):
                        elem.run_local(local_ws)
                    elif isinstance(elem, OtherSide):
                        elem.run(local_ws)
                    elems.remove(elem)
                    break
                except CantRun:
                    continue

        # Exit the PainterCluster: remove all local variables
        level_1_names = [Var.at_level(name, 1) for name in self.params()]
        result = local_ws.subst
        for level_1_name in level_1_names:
            result = result.remove(level_1_name)

        return result

    def elems_at_level(self, level: int) -> List[CompoundWorkspaceObj]:
        return [elem.with_var_level(level) for elem in self.elems]

    def eval(self, ws: Workspace) -> PainterCluster:
        return self

    # TODO Replace replace_constants_with_variables with a single, generic function that
    # examines the fields in the dataclass definition.

    def replace_constants_with_variables(self, ws: Workspace) \
    -> CompoundWorkspaceObj:
        return PainterCluster(
            *(elem.replace_constants_with_variables(ws) for elem in self.elems)
        )

    def __str__(self) -> str:
        cl = self.__class__.__name__
        elem_strs = ', '.join(short(elem, inside=True) for elem in self.elems)
        return f'{cl}({elem_strs})'

    __repr__ = __str__

Painter = Union[LengthPainter, Succ, Repeat, OtherSide, Type[Same], PainterCluster]
                
@dataclass(frozen=True)
class NoValue:
    '''When a variable is given NoValue() during construction of a
    PainterCluster, it does not become an element of the PainterCluster. It
    only serves as a common variable among different elements of the
    PainterCluster. The variable serves only to propagate values throughout
    workspace objects consistently, not to create a new workspace object of
    its own.'''
    pass

@dataclass
class DiffContext:
    '''A helper for Workspace.construct_diff(). Maintains a context that
    accumulates local variables and their definitions while constructing a
    PainterCluster that represents the difference between two WorkspaceObjs.'''
    ws: Workspace
    d: Dict[Variable, Union[WorkspaceObj, NoValue]] = \
        field(default_factory=dict)
    
    def define_var_for(self, var: Variable) -> Variable:
        new_var = self.new_variable_for(var)
        value = self.ws.eval(var)
        if value is None:
            raise Fizzle  # TODO indicate the problem
        else:
            self.d[new_var] = value
        return new_var

    def add_diff(self, var1: Variable, var2: Variable) \
    -> Tuple[Variable, Variable]:
        '''Creates variables for two arguments within something else.  If var1
        and var2 are the same, returns a single variable to stand for both. If
        var1 and var2 are different, returns two variables and creates and
        defines all variables needed for any arguments within var1 and and
        var2.'''
        value1 = self.ws.eval(var1)
        value2 = self.ws.eval(var2)

        assert value1 is not None
        assert value2 is not None
        if value1 == value2:  # If equal, make a single var for both
            result_var = self.new_variable_for(var1, prefer_no_suffix=True)
            self.d[result_var] = NoValue()
            return result_var, result_var
        else:  # else define two variables and link their values via some
               # representation of their difference
            match (value1, value2):
                case (CompoundWorkspaceObj() as v1,
                      CompoundWorkspaceObj() as v2):
                    assert value1.__class__ == value2.__class__
                    d1, d2 = self.make_argument_dicts_for(var1, var2)
                    result_var1 = self.define_new_variable_for(
                        var1, v1.__class__(**d1)
                    )
                    result_var2 = self.define_new_variable_for(
                        var2, v2.__class__(**d2)
                    )
                    return result_var1, result_var2
                case (Canvas(), Canvas()):
                    sidetag1 = extract_tag(SideTag, self.ws.tags_of(value1))
                    sidetag2 = extract_tag(SideTag, self.ws.tags_of(value2))
                    if SideTag.is_opposite_side(sidetag1, sidetag2):
                        result_var1 = \
                            self.define_new_variable_for(var1, NoValue())
                        result_var2 = \
                            self.define_new_variable_for(var2, NoValue())
                        otherside_var = self.define_new_variable_for(
                            'PP', OtherSide(result_var1, result_var2)
                        )
                        return result_var1, result_var2
                    else:
                        raise NotImplementedError
                case _:   # (_, _):
                    result_var1 = self.define_new_variable_for(var1, value1)
                    result_var2 = self.define_new_variable_for(var2, value2)
                    return result_var1, result_var2
                    
    #TODO rm (obsolete)
    def make_argument_variables_for(
        self,
        var1: Variable,
        var2: Variable
    ) -> Iterable[Tuple[Variable, Variable]]:
        '''Diffs all the arguments of value1 and value2 and returns an
        iterable of the pairs of variable names defined for them.'''
        value1: CompoundWorkspaceObj = self.ws[var1] # type: ignore[assignment]
        value2: CompoundWorkspaceObj = self.ws[var2] # type: ignore[assignment]
        for argvalue1, argvalue2 in zip(
            all_arguments_of(value1),
            all_arguments_of(value2)
        ):
            assert is_variable(argvalue1)
            assert is_variable(argvalue2)
            yield self.add_diff(argvalue1, argvalue2) 

    # var*        argvalue*
    # R1 = Repeat(S1, D1, F1)
    # R2 = Repeat(S2, D2, F2, E1)
    # NEXT Refactor to make the variables easier to understand
    def make_argument_dicts_for(
        self,
        var1: Variable,
        var2: Variable
    ) -> Tuple[Dict[Variable, Optional[Variable]],
               Dict[Variable, Optional[Variable]]]:
        d1: Dict[Variable, Optional[Variable]] = {}
        d2: Dict[Variable, Optional[Variable]] = {}
        value1: CompoundWorkspaceObj = self.ws[var1] # type: ignore[assignment]
        value2: CompoundWorkspaceObj = self.ws[var2] # type: ignore[assignment]
        assert value1.__class__ == value2.__class__
        for (param_name1, argvalue1), (param_name2, argvalue2) in zip(
            parameters_and_arguments_of(value1),
            parameters_and_arguments_of(value2)
        ):
            assert argvalue1 is None or is_variable(argvalue1)
            assert argvalue2 is None or is_variable(argvalue2)
            match (argvalue1, argvalue2):
                case (None, None):
                    continue
                case (v1, None):
                    d1[param_name1] = self.define_local_variable_and_value(
                        argvalue1, self.ws.eval(argvalue1)
                    )
                case (None, v2):
                    d2[param_name2] = self.define_local_variable_and_value(
                        argvalue2, self.ws.eval(argvalue2)
                    )
                case (v1, v2):
                    d1[param_name1], d2[param_name2] = self.add_diff(v1, v2)
        return d1, d2

    def elems(self) -> Iterable[PainterClusterElem]:
        for var, value in self.d.items():
            match value:
                case NoValue():
                    continue
                case OtherSide():
                    yield value
                case _:
                    yield Define(var, value)

    def define_new_variable_for(
        self,
        existing_variable: Variable,
        value: Union[WorkspaceObj, NoValue]
    ) -> Variable:
        new_variable = self.new_variable_for(existing_variable)
        self.d[new_variable] = value
        return new_variable

    def define_local_variable_and_value(
        self,
        existing_variable: Variable,
        value: Union[WorkspaceObj, NoValue]
    ) -> Variable:
        '''Defines a new, local variable for 'existing_variable' *and* defines
        new variables for all the arguments of 'value'.'''
        return self.define_new_variable_for(
            existing_variable,
            self.make_local_value(value)
        )

    def make_local_value(self, value: Union[WorkspaceObj, NoValue]) \
    -> Union[WorkspaceObj, NoValue]:
        '''Returns 'value', with, if appropriate, its arguments replaced by
        local variables, and defines those local variables to be the actual
        values of 'value's arguments.'''
        match value:
            case CompoundWorkspaceObj():
                # the recursive part
                return value.__class__(*(
                    self.define_local_variable_and_value(
                        argument_variable, argument_value
                    ) for argument_variable, argument_value in
                        self.object_argument_values_and_single_letter_names(
                            self.ws.eval(value)  # type: ignore[arg-type]
                        )
                ))
            case _:
                return value

    def object_argument_values_and_single_letter_names(
        self,
        value: CompoundWorkspaceObj
    ) -> Iterable[Tuple[Variable, WorkspaceObj]]:
        for argument_value in all_arguments_of(value):
            assert is_workspace_obj(argument_value)
            yield (
                single_letter_name_for(argument_value),
                argument_value
            )

    def new_variable_for(self, var: Variable, prefer_no_suffix: bool=False) \
    -> Variable:
        base_var = Var.doubled_first_letter(var)
        suffix = 0 if prefer_no_suffix else 1
        while True:
            result = Var.append_suffix(base_var, str(suffix)) \
                        if suffix >= 1 else base_var
            if result not in self.d:
                return result
            suffix += 1

def is_variable(x: Any) -> TypeGuard[Variable]:
    return (isinstance(x, str) and len(x) > 1) or isinstance(x, Var)

def is_base_obj(x: Any) -> TypeGuard[BaseObj]:
    return is_base_literal(x) or isinstance(x, Canvas) or is_type_op(x)

def is_letter(x: Any) -> TypeGuard[Letter]:
    return (isinstance(x, str) and len(x) == 1)

def is_base_literal(x: Any) -> TypeGuard[BaseLiteral]:
    return is_letter(x) or isinstance(x, int)

def is_type_op(x: Any) -> TypeGuard[Type[Op]]:
    return safe_issubclass(x, Op)

def is_workspace_obj(x: Any) -> TypeGuard[WorkspaceObj]:
    return is_base_obj(x) or isinstance(x, CompoundWorkspaceObj)

def is_painter(x: Any) -> TypeGuard[Painter]:
    return (
        is_type_op(x)
        or
        isinstance(x, (Op, LengthPainter, Repeat, OtherSide))
    )

@dataclass
class Workspace:
    subst: Subst = empty_subst
    var_counters: Dict[str, int] = \
        field(default_factory=lambda: defaultdict(int))
    _tags: Dict[Tag, Set[WorkspaceObj]] = \
        field(default_factory=lambda: defaultdict(set))
        # tag to the objects with that tag   TODO multiple tags, multiple objs
    _tags_of: Dict[WorkspaceObj, Set[Tag]] = \
        field(default_factory=lambda: defaultdict(set))
        # maps each elem to its tags

    def at(self, a: Address) -> Optional[WorkspaceObj]:
        match a:
            case CanvasAddress(canvas, i):
                canvas = self.get_canvas(canvas)
                i = self.get_index(i)
                return canvas[i]
            case ParameterAddress(painter, parameter_name):
                p = self.get_painter(painter)
                assert isinstance(parameter_name, str)  # TODO Allow a variable?
                return getattr(p, parameter_name)
            case _:
                raise NotImplementedError

    def parse_analogy_string(self, s: str) -> None:
        c1, c2, c3, c4 = Canvas.parse_analogy_string(s)
        self.define_and_name(c1, tags=[Lhs(), OldWorld()])
        self.define_and_name(c2, tags=[Rhs(), OldWorld()])
        self.define_and_name(c3, tags=[Lhs(), NewWorld()])
        self.define_and_name(c4, tags=[Rhs(), NewWorld()])

    def define(
        self,
        name: Variable,
        value: ARG, #Argument,
        tags: Tags=None
    ) -> ARG: #None:
        if isinstance(value, CompoundWorkspaceObj):
            value = value.replace_constants_with_variables(self) # type: ignore[assignment]
        if self._creating_a_new_object_within_a_painter_cluster(name, value):
            level_0_name = self.define_and_name(value) # type: ignore[arg-type]
            self.define(name, level_0_name)
        else:
            self.subst = self.subst.unify(name, value)
        for t in as_iter(tags):
            self._tags[t].add(value)  # type: ignore[arg-type]
            self._tags_of[value].add(t)  # type: ignore[index]
            #TODO How to ensure that the WorkspaceObj gets tagged, not just
            # variable that refers to it?--and the variable might not be set
            # to the WorkspaceObj until later.
        return value

    def _creating_a_new_object_within_a_painter_cluster(
        self,
        name: Variable,
        value: Argument
    ) -> bool:
        return (
            Var.is_at_least_level_1(name)
            and
            name not in self.subst
            and
            is_workspace_obj(value)
        )

    def unify(self, name: Variable, value: Argument) -> None:
        self.subst = self.subst.unify(name, value)

    def add_canvas(self, s: str) -> Canvas:
        canvas = Canvas.make_from(s)
        self.define_and_name(canvas)
        return canvas

    def define_and_name(self, obj: WorkspaceObj, tags: Tags=None) -> Variable:
        if (
            isinstance(obj, Canvas)
            and
            (existing_name := self.variable_of(obj)) is not None
        ):
            return existing_name # don't make a new name for an existing camnvas
        # Always returns a level-0 Variable
        name_letter = single_letter_name_for(obj)
        while True:
            self.var_counters[name_letter] += 1
            name = f'{name_letter}{self.var_counters[name_letter]}'
            if not name in self.subst:
                break
        self.define(name, obj, tags=tags)
        return name

    def __getitem__(self, obj: Argument) -> Optional[Argument]:
        '''Returns None if 'obj' is not defined. If 'obj' is defined as
        a variable name, returns the value of that other name. If 'obj'
        is just a WorkspaceObj and not a variable name, returns 'obj'.'''
        if is_variable(obj):
            value = self.subst.get(obj, None)
        else:
            value = obj
#        if is_variable(value):
#            return self[value]
#        else:
#            return value
        return value

    def variable_of(self, a: Argument) -> Optional[Variable]:
        for k, v in self.subst.items():
            if self.this_is_that(a, v):
                return k  # type: ignore[return-value]
        return None

    @classmethod
    def this_is_that(cls, a: Argument, v: Argument) -> bool:
        match a:
            case CompoundWorkspaceObj():
                return a is v
            case Canvas():
                return a is v
            case _:
                return a == v

    #def eval(self, x: Parameter[T]) -> Parameter[T]:
    def eval(self, x: Optional[Argument]) -> Optional[WorkspaceObj]:
        match x:
            case _ if is_variable(x):
                return self.eval(self[x])  # type: ignore[arg-type]
            case CompoundWorkspaceObj():
                return x.eval(self)
            case _:
                return x  # type: ignore[return-value]
        raise NotImplementedError  # Should never reach this line; mypy bug

    def tags_of(self, obj: Optional[Argument]) -> Tags:
        #return self._tags_of.get(self[obj], None)  # type: ignore[arg-type]
        return self._tags_of.get(self.eval(obj), None)  # type: ignore[arg-type]

    def find_objects_with_tag(self, tags: Tags) -> Set[WorkspaceObj]:
        return intersection(
            *(self._tags[t] for t in as_iter(tags))
        )

    def find_object_with_tag(self, tags: Tags) -> WorkspaceObj:
        # Fizzles if there is not exactly one object with the tags
        result = self.find_objects_with_tag(tags)
        if len(result) == 0:
            raise Fizzle(f'no object tagged {tags}')
        return first(result)

    def construct_diff(self, var1: Variable, var2: Variable, name=Variable) \
    -> PainterCluster:  # TODO Allow this to return other kinds of Painter
        context = DiffContext(self)
        context.add_diff(var1, var2)
        return PainterCluster(*context.elems())

    def run_repeater(self, p: Parameter[Repeat]) -> None:
        painter = self.get_repeater(p)  # TODO What about None?
        painter.fill(self)

    def run_painter(self, p: Parameter[Painter]) -> None:
        painter = self.get_painter(p)
        if isinstance(painter, (OtherSide, LengthPainter)):
            painter.run(self)
        else:
            raise NotImplementedError(painter)

    def run_painter_cluster(
        self, painter_cluster: Parameter[PainterCluster], subst_in: Subst
    ) -> None:
        match self[painter_cluster]:
            case PainterCluster() as pc:
                self.subst = pc.run(self, subst_in)
            case _:
                raise NotImplementedError  # TODO handle missing PainterCluster

    def run_detector(self, detector: Callable, *args) -> None:
        for obj in detector(self, *args):
            self.define_and_name(obj)

    def get_index(self, x: Parameter[Index]) -> Index:
        match x:
            case int():
                return x
            case _:
                result = self.eval(x)
                if isinstance(result, int):
                    return result
                else:
                    raise WrongType(Index, x, result)

    def get_canvas(self, x: Parameter[Canvas]) -> Canvas:
        match x:
            case Canvas():
                return x
            case _:
                result = self.eval(x)
                if isinstance(result, Canvas):
                    return result
                else:
                    raise WrongType(Canvas, x, result)

    def get_seed(self, x: Parameter[Seed]) -> Seed:
        match x:
            case Seed():
                return x
            case _:
                result = self.eval(x)
                if isinstance(result, Seed):
                    return result
                else:
                    raise WrongType(Seed, x, result)

    def get_op(self, x: Parameter[Type[Op]]) -> Type[Op]:
        match x:
            case type():
                return x
            case _:
                result = self.eval(x)
                if safe_issubclass(result, Op):
                    return result  # type: ignore[return-value]
                else:
                    raise WrongType(Op, x, result)

    def get_exception(self, x: OptionalParameter[Exception_]) \
    -> Optional[Exception_]:
        match x:
            case Exception_():
                return x
            case None:
                return None
            case _:
                result = self.eval(x)
                if result is None or isinstance(result, Exception_):
                    return result
                else:
                    raise WrongType(Exception_, x, result)

    def get_repeater(self, x: Parameter[Repeat]) -> Repeat:
        match x:
            case Repeat():
                return x
            case _:
                result = self.eval(x)
                if isinstance(result, Repeat):
                    return result
                else:
                    raise WrongType(Repeat, x, result)

    def get_painter(self, x: Parameter[Painter]) -> Painter:
        if is_painter(x):
            return x
        result = self.eval(x)
        if is_painter(result):
            return result
        raise WrongType(Painter, x, result)

    def get_painter_cluster(self, x: Parameter[PainterCluster]) \
    -> PainterCluster:
        match x:
            case PainterCluster():
                return x
            case _:
                result = self.eval(x)
                if isinstance(result, PainterCluster):
                    return result
                else:
                    raise WrongType(PainterCluster, x, result)

    def get_letter(self, x: Parameter[str]) -> str:
        result = self.eval(x)
        if is_letter(result):
            return result
        else:
            raise WrongType(Letter, x, result)

    def get_address(self, x: Parameter[Address]) -> Address:
        result = self.eval(x)
        if isinstance(result, Address):
            return result
        else:
            raise WrongType(Address, x, result)

    def all_letter_defs(self) -> Dict[Variable, Letter]:
        return dict(
            (k, v)  # type: ignore[misc]
                for k, v in self.subst.items()
                    if is_letter(v)
        )

    def all_seed_defs(self) -> Dict[Variable, Seed]:
        return dict(
            (k, v)  # type: ignore[misc]
                for k, v in self.subst.items()
                    if isinstance(v, Seed)
        )

    def all_index_defs(self) -> Dict[Variable, Index]:
        return dict(
            (k, v)  # type: ignore[misc]
                for k, v in self.subst.items()
                    if isinstance(v, int)
        )

    def get_all(self, typ: Optional[Type]=None) -> Iterable[WorkspaceObj]:
        match typ:
            case None:
                yield from self.subst.values()  # type: ignore[misc]
            case _:
                for item in self.subst.values():
                    if isinstance(item, typ):
                        yield item

    def short(self) -> str:
        return self.__class__.__name__

    __repr__ = short
    __str__ = short

BaseLiteral = Union[Letter, Index]
BaseObj = Union[BaseLiteral, Canvas, Type[Op]]
WorkspaceObj = Union[BaseObj, CompoundWorkspaceObj]
    # TODO in WorkspaceObj, change Type[Op] to Painter when Op inherits from Painter
Argument = Union[Variable, WorkspaceObj]
PainterClusterElem = Union[Define, OtherSide]  # TODO Add all Painters


if __name__ == '__main__':
    '''
    ws = Workspace()
    ws.parse_analogy_string('abc->abd; ijk->?')
    r1 = ws.define_and_name(detect_repetition(ws['S1']))
    r2 = ws.define_and_name(detect_repetition(ws['S2']))
    r3 = ws.define_and_name(detect_repetition(ws['S3']))
    arrow = ws.define_and_name(ws.construct_diff(r1, r2))
    ws.run_painter_cluster(arrow, Subst.from_kwargs(RR1=r3))

    ws.run_repeater(ws['R4'])
    print(ws['S4'])
    ws.subst.pr()
    '''
