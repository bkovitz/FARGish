# Pred.py

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from inspect import isclass

from util import as_dict, is_dataclass

# Something that can be converted into a predicate function whose only argument
# is the object in question.
Pred = Union[
    Type,
    Tuple,  # TODO should be Tuple[Pred, ...]
    Callable[[Any], bool],
    Hashable,  # Node
    None
]

# A predicate function (as opposed to something that can be turned into a
# predicate function, i.e. a Pred).
CallablePred = Callable[[Any], bool]


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
    #other_d = dataclasses.asdict(other)
    other_d = as_dict(other)
    #pr(other_d) #DEBUG
    #print('MWO', other == obj_template, id(other), id(obj_template), short(other), short(obj_template))
    #print()
    #pr(dataclasses.asdict(obj_template), key=short)
    #print()
#    for k, v in as_dict(obj_template).items():
#        otherv = other_d.get(k, None)
#        print(k, v, otherv, v == otherv)
#    print()
    #see(other, obj_template)
    result = all(
        (v is None or v == other_d.get(k, None) or k == 'id')
            #for k, v in dataclasses.asdict(obj_template).items()
            for k, v in as_dict(obj_template).items()
    )
    '''
    result = True
    for k, v in as_dict(obj_template).items():
        if not (v is None or v == other_d.get(k, None) or k == 'id'):
            print('FFF', v, other_d.get(k, None), k)
            result = False
            break
    '''
    #print('RESULT:', result)
    return result

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
