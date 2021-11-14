# FMTypes.py -- Fundamental 'typing' Type definitions for FARGModel and
# related classes and constants

from __future__ import annotations
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, \
    Iterator, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable
import dataclasses
from abc import ABC, abstractmethod
from inspect import isclass
from dataclasses import dataclass, is_dataclass

from util import pr, filter_none, dict_str, short


# Values with absolute value < epsilon are treated as zero
epsilon = 0.001 # 0.00001

# An element of the workspace or a node in the slipnet
#Elem = NewType('Elem', Hashable)
Elem = Hashable  # TODO rm
Elems = Union[Elem, Iterable[Elem], None]  # TODO rm

Node = Elem
Nodes = Elems

# A value that can be stored in a Canvas cell
#Value = NewType('Value', Hashable)
Value = Hashable

# The address of a Canvas cell within its Canvas
#Addr = NewType('Addr', Hashable)
Addr = Hashable

# Something that can be converted into a predicate function whose first
# argument is the FARGModel and whose second argument is the object in question
#FMPred = NewType('FMPred', Union[Type, Tuple, Callable, None])
FMPred = Union[
    Type,
    Tuple,  # should be Tuple[Pred, ...]
    Callable[['FARGModel', Any], bool],  # type: ignore[name-defined]
    Callable[[Any], bool],
    None
]
WSPred = Union[
    Type,
    Tuple,  # should be Tuple[Pred, ...]
    Callable[['Workspace', Any], bool],  # type: ignore[name-defined]
    Callable[[Any], bool],
    Node,
    None
]

# Something that can be converted into a predicate function whose only argument
# is the object in question.
Pred = Union[
    Type,
    Tuple,  # TODO should be Tuple[Pred, ...]
    Callable[[Any], bool],
    Node,
    None
]

# A predicate function (as opposed to something that can be turned into a
# predicate function, i.e. a Pred).
CallablePred = Callable[[Any], bool]

class HasBindWs(ABC):
    '''Mix-in to indicate that a class has a .bind_ws() method.'''

    def bind_ws(self, ws: 'Workspace') -> Pred:  # type: ignore[name-defined]
        '''Should return a Pred that does not take a Workspace as its first
        argument. If self needs a Workspace, the returned Pred should call
        self with 'ws' as the first argument.'''
        pass

def as_pred(o: Pred) -> CallablePred:
    if isclass(o):
        return IsInstance(o)  # type: ignore[arg-type]  # mypy bug?
    elif isinstance(o, tuple):
        return combine_preds(*o)
    elif callable(o):
        return o
    elif o is None:
        return AlwaysTrue()
    else:
        return MatchWoNone(o)

def combine_preds(*preds: Pred) -> CallablePred:
    preds: List[Pred] = [
        p for p in preds
            if p is not None and not isinstance(p, AlwaysTrue)
    ]
    if not preds:
        return AlwaysTrue()
    elif len(preds) == 1:
        return as_pred(preds[0])
    else:
        preds_to_or: List[Pred] = []
        preds_to_and_first: List[Pred] = []
        for p in preds:
            if isinstance(p, AndFirst):
                preds_to_and_first.append(p)
            else:
                preds_to_or.append(p)
        if not preds_to_or:
            return AndPreds(*preds_to_and_first)
        elif not preds_to_and_first:
            return OrPreds(*preds_to_or)
        else:
            return AndPreds(*preds_to_and_first, OrPreds(*preds_to_or))

@dataclass(frozen=True)
class IsInstance:
    cl: Type

    def __call__(self, x: Any) -> bool:
        return isinstance(x, self.cl)

@dataclass(frozen=True)
class AlwaysTrue:

    def __call__(self, x: Any) -> bool:
        return True

# TODO UT
@dataclass(frozen=True)
class Not:
    pred: CallablePred

    def __call__(self: Not, x: Any) -> bool:
        return not self.pred(x)  # type: ignore[operator]  # mypy bug?

@dataclass(frozen=True)
class AndPreds:
    preds: Tuple[CallablePred, ...]

    def __init__(self, *preds: Pred):
        object.__setattr__(self, 'preds', tuple(as_pred(p) for p in preds))

    def __call__(self, x: Any) -> bool:
        return all(p(x) for p in self.preds)

@dataclass(frozen=True)
class OrPreds:
    preds: Tuple[CallablePred, ...]

    def __init__(self, *preds: Pred):
        object.__setattr__(self, 'preds', tuple(as_pred(p) for p in preds))

    def __call__(self, x: Any) -> bool:
        return any(p(x) for p in self.preds)

    # TODO rm
    @classmethod
    def make(cls, *preds: Pred) -> CallablePred:
        return OrPreds(tuple(as_pred(p) for p in preds))

def match_wo_none(other, obj_template) -> bool:
    '''Does obj_template == other if we ignore any fields in obj_template
    with a value of None? If other is an object of a subclass of obj_template's
    class, that also counts as a match.'''
    if not isinstance(other, obj_template.__class__):
        return False
    if not is_dataclass(obj_template) or not is_dataclass(other):
        return obj_template == other
    other_d = dataclasses.asdict(other)
    return all(
        v is None or v == other_d.get(k, None)
            for k, v in dataclasses.asdict(obj_template).items()
    )

@dataclass(frozen=True)
class MatchWoNone:
    obj_template: Hashable

    def __call__(self, obj: Any) -> bool:
        return match_wo_none(obj, self.obj_template)

class AndFirst:
    '''Mix-in to indicate a CallablePred that, if it returns false, should
    cause the whole collection of Preds that it contains to return false,
    without calling other Preds in the collection.'''
    pass

@dataclass(frozen=True)
class Exclude(HasBindWs, AndFirst):
    obj_template: Hashable

    def __call__(self, obj: Any) -> bool:
        return not match_wo_none(obj, self.obj_template)

# An activation level
Activation = float

# A dictionary mapping things to activation levels
ADict = Dict[Hashable, Activation]
