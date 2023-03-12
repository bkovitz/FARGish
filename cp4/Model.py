# Model.py -- The canvas-and-painters model

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod
from itertools import chain
from collections import defaultdict
import re

from pyrsistent import pmap
from pyrsistent.typing import PMap

from Log import lo, trace
from util import as_iter, first, force_setattr, intersection, short, union


T = TypeVar('T')

@dataclass(frozen=True)
class Var:
    '''A variable, together with a level number so that @@'''
    name: str
    level: int

    @classmethod
    def at_level(cls, v: Union[str, Var, T], level: int) -> Union[Var, T]:
        match v:
            case str() if is_variable(v):
                return Var(v, level)  # type: ignore[arg-type]
            case Var():
                return Var(v.name, level)
            case WorkspaceElem():
                return v.with_var_level(level)  # type: ignore[return-value]
            case _:
                return v  # type: ignore[return-value]
        return v  # type: ignore[return-value]   # mypy bug

Variable = Union[str, Var]
Parameter = Union[Variable, T]
OptionalParameter = Union[str, T, None]

Subst = Dict[Variable, Parameter]

Index = int

class WorkspaceElem(ABC):

    @abstractmethod
    def params(self) -> Collection[Variable]:
        pass

    @abstractmethod
    def with_var_level(self, level: int) -> WorkspaceElem:
        '''Return a new WorkspaceElem, in which all the Parameter variables have been
        replaced with Var objects at 'level'.'''
        pass

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

########## Fizzles ##########

@dataclass(frozen=True)
class Fizzle(Exception):
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

    def __str__(self) -> str:
        return self.contents

    def short(self) -> str:
        return repr(self.contents)

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

    # NEXT Rewrite .make() top-down

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
                    return cls.generate_next(seed_letter, i)
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

class Succ(Op):

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

class Exception_(WorkspaceElem):
    pass

@dataclass(frozen=True)
class Skip(Exception_):
    i: Parameter[Index]

    def params(self) -> List[Variable]:
        result: List[Variable] = []
        if is_variable(self.i):
            result.append(self.i)
        return result

    def with_var_level(self, level: int) -> Skip:
        return Skip(Var.at_level(self.i, level))

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
class Seed(WorkspaceElem):
    letter: Parameter[str]
    i: Parameter[Index]

    def get_i(self, ws: Workspace) -> Index:
        return ws.get_index(self.i)

    def get_letter(self, ws: Workspace) -> str:
        return ws.get_letter(self.letter)

    def params(self) -> List[Variable]:
        result: List[Variable] = []
        if is_variable(self.letter):
            result.append(self.letter)
        if is_variable(self.i):
            result.append(self.i)
        return result

    def with_var_level(self, level: int) -> Seed:
        return Seed(
            Var.at_level(self.letter, level),
            Var.at_level(self.i, level)
        )

    def __repr__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({self.letter!r}, {self.i})'

#class DeterminateSeed:
#    letter: str
#    i: Index
    

@dataclass(frozen=True)
class Repeat:
    canvas: Parameter[Canvas]
    seed: Parameter[Seed]
    op: Parameter[Type[Op]]
    exception: OptionalParameter[Skip] = None

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

@dataclass(frozen=True)
class OtherSide(WorkspaceElem):
    left: Variable     # TODO allow a constant?
    right: Variable    # TODO allow a constant?

    def run(self, ws: Workspace) -> None:
#        if isinstance(self.right, str):
#            #ws.define(self.right, 'S2')
#            #ws.define(self.right, ws.find_object_with_tag(Rhs()))
#            if (world := ws.world_of(self.left)) is not None:
#                ws.define(
#                    self.right,
#                    ws.find_object_with_tag([Rhs(), world])
#                )

        match (ws[self.left], ws[self.right]):
            case (Canvas(), None):
                mate = ws.find_object_with_tag(self.other_side_tags(ws, self.left))
                ws.define(self.right, mate)
                # TODO what if mate does not exist?
            case (None, Canvas()):
                mate = ws.find_object_with_tag(self.other_side_tags(ws, self.right))
                ws.define(self.left, mate)
                # TODO what if mate does not exist?
            # TODO the failure cases
                
    #def other_side_tags(self, tags: Tags) -> Tags:
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

    def params(self) -> List[Variable]:
        return [self.left, self.right]

    def with_var_level(self, level: int) -> OtherSide:
        return OtherSide(
            Var.at_level(self.left, level),
            Var.at_level(self.right, level),
        )

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

Painter = Union[Repeat, OtherSide]
                

@dataclass(frozen=True)
class Define(WorkspaceElem):
    '''Contrary to the name of what Define inherits from, a Define may not exist as
    a full citizen of the Workspace. It may only be an element of a PainterCluster.'''
    name: Variable
    value: Any

    def with_variables_at_level(self) -> Define:
        pass

    def params(self) -> List[Variable]:
        result: List[Variable] = [self.name]
        if is_variable(self.value):
            result.append(self.value)
        else:
            if isinstance(self.value, WorkspaceElem):
                result += self.value.params()
        return result

    def with_var_level(self, level: int) -> Define:
        return Define(
            Var.at_level(self.name, level),
            self.value  #self.value.with_var_level(level)
        )

    def run_local(self, local_ws: Workspace) -> None:
        #lo('RLO', self.name in local_ws.subst, local_ws.subst)
        if self.name not in local_ws.subst:
            #lo('DEFINE', self.name, self.value)
            local_ws.define(self.name, self.value)
        

@dataclass(frozen=True)
class PainterCluster(WorkspaceElem):
    elems: Tuple[WorkspaceElem, ...]

    def __init__(self, *elems: WorkspaceElem):
        force_setattr(self, 'elems', elems)

    def params(self) -> Set[Variable]:
        return union(*(elem.params() for elem in self.elems))

    def with_var_level(self, level: int) -> PainterCluster:
        return self  # TODO Is this right? Or should we make a new PainterCluster
                     # with new elems?

    def run(self, ws_in: Workspace, subst_in: Subst) -> None:
        subst_in: Subst = dict(
            (Var.at_level(k, 1), v) for k, v in subst_in.items()
        )
        local_ws = Workspace(subst=ws_in.subst | subst_in)
        elems = self.elems_at_level(1)
        for elem in elems:
            if isinstance(elem, Define):
                elem.run_local(local_ws)

        # Copy local variables back to ws_in
        for name_in in [Var.at_level(name, 1) for name in self.params()]:
            if name_in not in subst_in:  # if name was not given as an argument
                ws_in.define_letter(local_ws[name_in])

    def elems_at_level(self, level: int) -> List[WorkspaceElem]:
        return [elem.with_var_level(level) for elem in self.elems]

def is_variable(x: Any) -> TypeGuard[Variable]:
    return (isinstance(x, str) and len(x) > 1) or isinstance(x, Var)

@dataclass
class Workspace:
    subst: Dict[Variable, Any] = field(default_factory=dict)
        # a "substitution": a map of variable names to values
    _tags: Dict[Tag, Set[WorkspaceElem]] = \
        field(default_factory=lambda: defaultdict(set))
        # tag to the objects with that tag   TODO multiple tags, multiple objs
    _tags_of: Dict[WorkspaceElem, Set[Tag]] = \
        field(default_factory=lambda: defaultdict(set))
        # maps each elem to its tags
    letter_var_counter: int = 0
    
    def define(
        self, name: Variable, value: Any, tag: Union[Tag, List[Tag], None]=None
    ) -> None:
        self.subst[name] = value
        for t in as_iter(tag):
            self._tags[t].add(value)
            self._tags_of[value].add(t)

    def define_letter(self, letter: str) -> Variable:
        while True:
            self.letter_var_counter += 1
            name = f'L{self.letter_var_counter}'
            if not name in self.subst:
                break
        self.define(name, letter)
        return name

    def undefine(self, name: str) -> None:
        '''It is not an error to undefine an undefined variable.'''
        if name in self.subst:
            del self.subst[name]
        # TODO rm tags

    def tags_of(self, obj: Parameter[WorkspaceElem]) -> Tags:
        return self._tags_of.get(self[obj], None)

    def find_objects_with_tag(self, tags: Tags) -> Set[WorkspaceElem]:
        return intersection(
            *(self._tags[t] for t in as_iter(tags))
        )

    def find_object_with_tag(self, tags: Tags) -> WorkspaceElem:
        # Fizzles if there is not exactly one object with the tags
        result = self.find_objects_with_tag(tags)
        if len(result) == 0:
            raise Fizzle()  # TODO indicate reason for fizzle
        return first(result)

    def extract_tag(self, tagtype: Type[Tag], obj: WorkspaceElem) \
    -> Optional[Tag]:
        return extract_tag(tagtype, self._tags_of.get(self[obj], None))

    def __getitem__(self, obj: Parameter[WorkspaceElem]) -> Any:
        '''Returns None if 'obj' is not defined. If 'obj' is defined as
        a variable name, returns the value of that other name.'''
        if is_variable(obj):
            value = self.subst.get(obj, None)
        else:
            value = obj
        if is_variable(value):
            return self[value]
        else:
            return value

    def all_letter_defs(self) -> Dict[Variable, str]:
        return dict(
            (k, v)
                for k, v in self.subst.items()
                    if isinstance(v, str)
        )

    # TODO Merge this into run_painter
    def run_repeater(self, p: Parameter[Repeat]) -> None:
        painter = self.get_repeater(p)  # TODO What about None?
        painter.fill(self)

    def run_painter(self, p: Parameter[Painter]) -> None:
        if isinstance(p, OtherSide):
            p.run(self)

    def run_painter_cluster(
        self, painter_cluster: Parameter[PainterCluster], subst_in: Subst
    ) -> None:
        match (pc := self[painter_cluster]):
            case PainterCluster():
                pc.run(self, subst_in)
            case _:
                raise NotImplementedError  # TODO handle missing PainterCluster
#        #painter_cluster = self.get_painter_cluster(pc)
#        #painter_cluster.run(self, subst)
#        subst: Subst = {}
#        for k,v in subst_in.items():
#            subst[k] = self[v]
#        return subst

    def get_index(self, x: Parameter[Index]) -> Index:
        match x:
            case int():
                return x
            case _:
                return self[x]

    def get_canvas(self, x: Parameter[Canvas]) -> Canvas:
        match x:
            case Canvas():
                return x
            case _:
                return self[x]

    def get_seed(self, x: Parameter[Seed]) -> Seed:
        match x:
            case Seed():
                return x
            case _:
                return self[x]

    def get_op(self, x: Parameter[Type[Op]]) -> Type[Op]:
        match x:
            case type():
                return x
            case _:
                return self[x]

    def get_exception(self, x: OptionalParameter[Exception_]) \
    -> Optional[Exception_]:
        match x:
            case Exception_():
                return x
            case None:
                return None
            case _:
                return self[x]

    def get_repeater(self, x: Parameter[Repeat]) -> Repeat:
        match x:
            case Repeat():
                return x
            case _:
                return self[x]

    def get_painter_cluster(self, x: Parameter[PainterCluster]) \
    -> PainterCluster:
        match x:
            case PainterCluster():
                return x
            case _:
                return self[x]

    def get_letter(self, x: Parameter[str]) -> str:
        if is_variable(x):
            return self[x]
        else:
            return x    # type: ignore[return-value]

    def short(self) -> str:
        return self.__class__.__name__
