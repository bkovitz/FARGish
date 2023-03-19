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

from pyrsistent import pmap
from pyrsistent.typing import PMap

from Log import lo, trace
from util import as_iter, field_names_and_values, first, force_setattr, \
    intersection, safe_issubclass, short, union


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
            case CompoundWorkspaceObj():
                return v.with_var_level(level)  # type: ignore[return-value]
            case _:
                return v  # type: ignore[return-value]
        return v  # type: ignore[return-value]   # mypy bug


Variable = Union[str, Var]
Parameter = Union[Variable, T]
OptionalParameter = Union[Variable, T, None]

VDict = Dict[Variable, Parameter]

Letter = str  # strictly speaking, a single-character, lowercase str
Index = int

@dataclass(frozen=True)
class CompoundWorkspaceObj(ABC):

    @abstractmethod
    def params(self) -> Collection[Variable]:
        pass

    @abstractmethod
    def with_var_level(self, level: int) -> CompoundWorkspaceObj:
        '''Return a new CompoundWorkspaceObj, in which all the Parameter variables have been
        replaced with Var objects at 'level'.'''
        pass

    @abstractmethod
    def eval(self: T, ws: Workspace) -> T:
        pass

    @abstractmethod
    def replace_constants_with_variables(self, ws: Workspace) -> CompoundWorkspaceObj:
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

########## Subst ##########

@dataclass(frozen=True)
class Subst:
    d: PMap[Argument, Argument] = field(default_factory=pmap)

    def unify(self, a1: Argument, a2: Argument) -> Subst:
        return Subst(self.d.set(a1, a2))

    def is_bottom(self) -> bool:
        return False

    def eval(self, a: Argument) -> Optional[WorkspaceObj]:
        match a:
            case _ if is_variable(a):
                match (v := self.d.get(a)):
                    case None:
                        return None
                    case _:
                        return self.eval(v)
            case str():
                return a
            case int():
                return a
        raise NotImplementedError

    def are_equal(self, a1: Argument, a2: Argument) -> bool:
        match a1:
            case _ if is_variable(a1):
                if a1 not in self.d:
                    return False
                #return self.d.get(a1) == self.d.get(a2)
        return True

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

class Exception_(CompoundWorkspaceObj):
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

    def eval(self, ws: Workspace) -> Skip:
        raise NotImplementedError  # TODO

    def replace_constants_with_variables(self, ws: Workspace) \
    -> CompoundWorkspaceObj:
        if is_variable(self.i):
            return self
        else:
            return Skip(ws.define_and_name(self.i))  # type: ignore[arg-type]

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

    def eval(self, ws: Workspace) -> Seed:
        return Seed(
            ws.eval(self.letter),
            ws.eval(self.i)
        )

    def replace_constants_with_variables(self, ws: Workspace) -> CompoundWorkspaceObj:
        if is_variable(self.letter) and is_variable(self.i):
            return self
        else:
            letter_variable = (
                ws.define_and_name(self.letter) # type: ignore[arg-type]
                    if not is_variable(self.letter) else self.letter
                )
            i_variable = (
                ws.define_and_name(self.i) # type: ignore[arg-type]
                    if not is_variable(self.i) else self.i
                )
            #lo('REPL', letter_variable, i_variable)
            return Seed(letter_variable, i_variable)

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

    def eval(self, ws: Workspace) -> Repeat:
        raise NotImplementedError  # TODO

    def with_var_level(self, level: int) -> Repeat:
        return Repeat(
            Var.at_level(self.canvas, level),
            Var.at_level(self.seed, level),
            Var.at_level(self.op, level),
            Var.at_level(self.exception, level)
        )

    def params(self) -> List[Variable]:
        return [
            x
                for x in [self.canvas, self.seed, self.op, self.exception]
                    if is_variable(x)
        ]

    def replace_constants_with_variables(self, ws: Workspace) -> CompoundWorkspaceObj:
        if (
            is_variable(self.canvas)
            and
            is_variable(self.seed)
            and
            is_variable(self.op)
            and
            (self.exception is None or is_variable(self.exception))
        ):
            return self
        else:
            canvas_variable = (
                ws.define_and_name(self.canvas) # type: ignore[arg-type]
                    if not is_variable(self.canvas) else self.canvas
            )
            seed_variable = (
                ws.define_and_name(self.seed) # type: ignore[arg-type]
                    if not is_variable(self.seed) else self.seed
            )
            op_variable = (
                ws.define_and_name(self.op) # type: ignore[arg-type]
                    if not is_variable(self.op) else self.op
            )
            if self.exception is None:
                exception_variable = None
            elif is_variable(self.exception):
                exception_variable = self.exception
            else:
                exception_variable = ws.define_and_name(self.exception) # type: ignore[arg-type]
            return Repeat(
                canvas_variable, seed_variable, op_variable, exception_variable
            )

@dataclass(frozen=True)
class OtherSide(CompoundWorkspaceObj):
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

Painter = Union[Repeat, OtherSide]
                

@dataclass(frozen=True)
class Define(CompoundWorkspaceObj):
    '''Contrary to the name of what Define inherits from, a Define may not exist as
    a full citizen of the Workspace. It may only be an element of a PainterCluster.'''
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
        #lo('RLO', self.name in local_ws.subst, local_ws.subst)
        if self.name not in local_ws.subst:
            #lo('DEFINE', self.name, self.value)
            #local_ws.define(self.name, self.value)
            #lo('LOCAL WS', local_ws.subst)
            #local_ws.define(self.name, local_ws.eval(self.value))
            local_ws.define(self.name, self.value)
        
    def eval(self, ws: Workspace) -> Define:
        raise NotImplementedError  # TODO

    def replace_constants_with_variables(self, ws: Workspace) -> CompoundWorkspaceObj:
        return self  # DOUBT Is this right? Do we need to replace constants inside
                     # self.value?

def is_type_op(x: Any) -> TypeGuard[Type[Op]]:
    return safe_issubclass(x, Op)

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

def try_to_make_level_0(subst: VDict, o: CompoundWorkspaceObj) \
-> Optional[CompoundWorkspaceObj]:
    d: Dict[str, Any] = {}
    #for k, v in names_and_arguments_of(o):
    for name, v in parameters_and_arguments_of(o):
        if is_base_obj(v):
            continue
        elif is_variable(v):    # TODO what if v is level-0?
            if is_level_0(v):
                continue
            else:
                # NEXT If vv is not a variable, give up
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
            if value is not None:
                yield (name, value)
    
def all_arguments_of(o: CompoundWorkspaceObj) -> Iterable[Argument]:
    for name, typ in get_type_hints(o.__class__).items():
        if is_parameter(typ):
            value = getattr(o, name)
            if value is not None:
                yield value

@dataclass(frozen=True)
class PainterCluster(CompoundWorkspaceObj):
    elems: Tuple[PainterClusterElem, ...]

    def __init__(self, *elems: CompoundWorkspaceObj):
        force_setattr(self, 'elems', elems)

    def params(self) -> Set[Variable]:
        return union(*(elem.params() for elem in self.elems))

    def with_var_level(self, level: int) -> PainterCluster:
        return self # TODO Is this right? Or should we make a new PainterCluster
                    # with new elems?

    def run(self, ws_in: Workspace, subst_in: VDict) -> VDict:
        subst_in: VDict = dict(
            (Var.at_level(k, 1), v) for k, v in subst_in.items()
        )
        local_ws = Workspace(subst=ws_in.subst | subst_in)
        elems = self.elems_at_level(1)
        for elem in elems:
            if isinstance(elem, Define):
                elem.run_local(local_ws)

#        lo('LOCAL_WS')
#        for k, v in local_ws.subst.items():
#            print(f'{k!r}: {v!r}')
#        # Copy local variables back to ws_in
#        for name_in in [Var.at_level(name, 1) for name in self.params()]:
#            if name_in not in subst_in:  # if name was not given as an argument
#                ws_in.define_and_name(local_ws[name_in])



        # Make a map of level-1 names to level-0 names
        # While making the map, if the level-1 name doesn't correspond to any
        # level-0 object, we create a level-0 object for it, with a level-0 name.

        # fix level-1 names in arguments of CompoundWorkspaceObjs
        # e.g. replace Seed(LL, II) with Seed(L1, I1)
        #           DD=Seed(LL, II)   D1=Seed(L1, I1)

        # clear level-1 names
#        for name_level_1, v1 in subst_in.items():
#            self.remove_level_1_name(name_level_1, v1)


        # Approach #2:
        # Walk through subst_in and make a modification to ws for each k,v.

        # Approach #3:  THE WINNER
        #
        # 1. Make a map of level-1 names to level-0 names
        #    DD -> D1    This is hard because D1 does not exist yet, so we don't
        #                even know what .define_and_name() will name it.
        #    LL -> L1
        #    II -> I1
        # While making the map, if the level-1 name doesn't correspond to any
        # level-0 object, we create a level-0 object for it, with a level-0 name.
        # If we can't create the level-0 object yet, because it includes a level-1
        # object that we don't have a level-0 equivalent for yet, then we defer it
        # for later and try mapping another level-1 variable.
        #
        # 2. Throw away local_ws.

        # Approach #4:
        # Only create level-0 objects, and give them level-0 names when creating them.
        # During the running of the PainterCluster, maintain a map of level-1 names
        # to level-0 names.
        # Inside the PainterCluster, we need common level-1 names in different
        # objects to refer to the same things.

        subst_out = local_ws.subst.copy()
        level_1_names = [Var.at_level(name, 1) for name in self.params()]
        while level_1_names:
            name1 = level_1_names.pop(0)  # the level-1 name we're trying to eliminate
            #value1 = subst_in[name1] # its value inside the PainterCluster
            value1 = subst_out.get(name1, None) # its value inside the PainterCluster
            #lo('LOOP', name1, value1)
            #import pdb; pdb.set_trace()
            match value1:
                case None:      # name1 never got defined
                    break       # so give up  (TODO maybe Fizzle)
                case str() if is_variable(value1):
                                      # name1 is defined as a level-0 name
                    continue          # so, we're done with name1
                case Var(level=1):    # name1 is defined as a level-1 name
                    value2 = subst_in[name1]
                    if is_level_0(value2):
                        subst_out[name1] = value2  # redefine name1 to bypass level-1
                    # we'll need a second pass for name1
                case str():     # name1 is defined as a letter
                    level_0_name = ws_in.define_and_name(value1)
                    subst_out[name1] = level_0_name
                    continue
                case int():     # name1 is defined as an index
                    level_0_name = ws_in.define_and_name(value1)
                    subst_out[name1] = level_0_name
                    continue
                case Canvas():  # name1 is defined as a canvas
                    level_0_name = ws_in.define_and_name(value1)
                    subst_out[name1] = level_0_name
                    continue
                case _ if is_type_op(value1):
                    level_0_name = ws_in.define_and_name(value1)
                    subst_out[name1] = level_0_name
                    continue
                case CompoundWorkspaceObj():  # name1 is defined as a compound object
                    value0 = try_to_make_level_0(subst_out, value1)
                    if value0 is None:
                        pass  # wait for another iteration
                    else:
                        level_0_name = ws_in.define_and_name(value0)
                        subst_out[name1] = level_0_name
                        continue
                            
                    # else name1 must wait for another iteration of the loop, after
                    # the members of value1 have received level-0 definitions.

            level_1_names.append(name1)


            '''
            if name1 is defined entirely in terms of level-0 names, we're done with it.
            if name1 is defined as a level-1 name, 
                if that name has a level-0 definition:
                    define name1 as that level-0 definition
                else
                    level_1_names.append(name1)
            if name1 is defined as an object:
                if the object is entirely level-0:
                    create that object in ws and give it a level-0 name
                    define name1 in subst_in as that level-0 name
                elif the object depends only on level-1 names:
                    if all those names are defined
                        create a new object in ws with names mapped to level-0
                        define name1 in subst_in as that object's level-0 name
                ...else:
                    level_1_names.append(name1)
                    
            '''

        # Now we simply throw away local_ws and subst_in. All new contents have been
        # copied to ws and given new names, whose members refer to each other
        # isomorphically to the names in subst_in and local_ws.
        return subst_out

        # Hypothesis: If a level-1 name is not defined as a level-0 name, does that
        # mean that the level-1 name refers to a new object? If so, then we know
        # that we must create a level-0 object for it.


        # WANT on exit:
        #   Any new object created in the PainterCluster should exist in ws and
        #   have a level-0 name.
        #
        #   All level-1 variables should be gone.

        # On return:
        #   Replace all level-1 variables with their values (which could be
        #   level-0 variables). This occurs inside CompoundWorkspaceObjs.
        #
        #   Create new objects, with level-0 variable names, for objects created
        #   inside the PainterCluster. Or maybe just give these new objects new
        #   names, at level-0.

        # Replace replace_constants_with_variables with a single, generic function that
        # examines the fields in the dataclass definition.

    def elems_at_level(self, level: int) -> List[CompoundWorkspaceObj]:
        return [elem.with_var_level(level) for elem in self.elems]

    def eval(self, ws: Workspace) -> PainterCluster:
        raise NotImplementedError  # TODO

    def replace_constants_with_variables(self, ws: Workspace) -> CompoundWorkspaceObj:
        return PainterCluster(
            *(elem.replace_constants_with_variables(ws) for elem in self.elems)
        )

def is_variable(x: Any) -> TypeGuard[Variable]:
    return (isinstance(x, str) and len(x) > 1) or isinstance(x, Var)

def is_base_obj(x: Any) -> TypeGuard[BaseObj]:
    return is_base_literal(x) or isinstance(x, Canvas) or is_type_op(x)

def is_base_literal(x: Any) -> TypeGuard[BaseLiteral]:
    return (isinstance(x, str) and len(x) == 1) or isinstance(x, int)

@dataclass
class Workspace:
    subst: Dict[Variable, Any] = field(default_factory=dict)
        # a "substitution": a map of variable names to values
    #_tags: Dict[Tag, Set[Variable]] = \
    _tags: Dict[Tag, Set[WorkspaceObj]] = \
        field(default_factory=lambda: defaultdict(set))
        # tag to the objects with that tag   TODO multiple tags, multiple objs
    #_tags_of: Dict[Variable, Set[Tag]] = \
    _tags_of: Dict[WorkspaceObj, Set[Tag]] = \
        field(default_factory=lambda: defaultdict(set))
        # maps each elem to its tags
    #letter_var_counter: int = 0
    var_counters: Dict[str, int] = field(default_factory=lambda: defaultdict(int))
    
    def define(
        self, name: Variable, value: Argument, tag: Union[Tag, List[Tag], None]=None
    ) -> None:
#        for t in as_iter(tag):
#            self._tags[t].add(value)
#            self._tags_of[value].add(t)
        v = self[value]
        if isinstance(v, CompoundWorkspaceObj):
            v = v.replace_constants_with_variables(self)
        self.subst[name] = v
        for t in as_iter(tag):
            self._tags[t].add(v)
            self._tags_of[v].add(t)

#        match value:
#            case _ if is_literal(value):
#                self.subst[name] = self.value
#            case CompoundWorkspaceObj():
#                value = 
#        for k, v in 

#    def define_letter(self, letter: str) -> Variable:
#        while True:
#            self.letter_var_counter += 1
#            name = f'L{self.letter_var_counter}'
#            if not name in self.subst:
#                break
#        self.define(name, letter)
#        return name

    def define_and_name(self, obj: WorkspaceObj) -> Variable:
        name_letter: str
        match obj:
            case str():   # Letter
                name_letter = 'L'
            case Seed():
                name_letter = 'D'
            case int():
                name_letter = 'I'
            case _ if is_type_op(obj):
                name_letter = 'F'
            case Repeat():
                name_letter = 'R'
            case _:
                raise NotImplementedError(obj)
        while True:
            self.var_counters[name_letter] += 1
            name = f'{name_letter}{self.var_counters[name_letter]}'
            if not name in self.subst:
                break
        self.define(name, obj)
        return name

    def undefine(self, name: str) -> None:
        '''It is not an error to undefine an undefined variable.'''
        if name in self.subst:
            del self.subst[name]
        # TODO rm tags

    def tags_of(self, obj: Parameter[CompoundWorkspaceObj]) -> Tags:
        return self._tags_of.get(self[obj], None)

    def find_objects_with_tag(self, tags: Tags) -> Set[CompoundWorkspaceObj]:
        return intersection(
            *(self._tags[t] for t in as_iter(tags))
        )

    def find_object_with_tag(self, tags: Tags) -> CompoundWorkspaceObj:
        # Fizzles if there is not exactly one object with the tags
        result = self.find_objects_with_tag(tags)
        if len(result) == 0:
            raise Fizzle()  # TODO indicate reason for fizzle
        return first(result)

    def extract_tag(self, tagtype: Type[Tag], obj: CompoundWorkspaceObj) \
    -> Optional[Tag]:
        return extract_tag(tagtype, self._tags_of.get(self[obj], None))

    def __getitem__(self, obj: Argument) -> Any:
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

#    def eval(self, x: Parameter[ CompoundWorkspaceObj | str | Index]) \
#    -> Parameter[CompoundWorkspaceObj | str | Index]:
    def eval(self, x: Parameter[T]) -> Parameter[T]:
        match x:
            case _ if is_variable(x):
                return self[x]
            case CompoundWorkspaceObj():
                return x.eval(self)  # type: ignore[return-value]
            case _:
                return x
        raise NotImplementedError  # Should never reach this line; mypy bug

    def all_letter_defs(self) -> Dict[Variable, str]:
        return dict(
            (k, v)
                for k, v in self.subst.items()
                    if isinstance(v, str)
        )

    def all_seed_defs(self) -> Dict[Variable, Seed]:
        return dict(
            (k, v)
                for k, v in self.subst.items()
                    if isinstance(v, Seed)
        )

    def all_index_defs(self) -> Dict[Variable, int]:
        return dict(
            (k, v)
                for k, v in self.subst.items()
                    if isinstance(v, int)
        )

    # TODO Merge this into run_painter
    def run_repeater(self, p: Parameter[Repeat]) -> None:
        painter = self.get_repeater(p)  # TODO What about None?
        painter.fill(self)

    def run_painter(self, p: Parameter[Painter]) -> None:
        if isinstance(p, OtherSide):
            p.run(self)

    def run_painter_cluster(
        self, painter_cluster: Parameter[PainterCluster], subst_in: VDict
    ) -> VDict:
        match (pc := self[painter_cluster]):
            case PainterCluster():
                return pc.run(self, subst_in)
            case _:
                raise NotImplementedError  # TODO handle missing PainterCluster
#        #painter_cluster = self.get_painter_cluster(pc)
#        #painter_cluster.run(self, subst)
#        subst: VDict = {}
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

BaseLiteral = Union[Letter, Index]
BaseObj = Union[BaseLiteral, Canvas, Type[Op]]
WorkspaceObj = Union[BaseObj, CompoundWorkspaceObj]
    # TODO in WorkspaceObj, change Type[Op] to Painter when Op inherits from Painter
Argument = Union[Variable, WorkspaceObj]
PainterClusterElem = Union[Define]  # TODO Add all Painters
