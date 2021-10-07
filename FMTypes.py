# FMTypes.py -- Fundamental 'typing' Type definitions for FARGModel and
# related classes and constants

from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence, Literal, Protocol, runtime_checkable
from inspect import isclass
from dataclasses import is_dataclass
import dataclasses


# Values with absolute value < epsilon are treated as zero
epsilon = 0.00001

# An element of the workspace or a node in the slipnet
#Elem = NewType('Elem', Hashable)
Elem = Hashable
Elems = Union[Elem, Iterable[Elem], None]

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

# Something that can be converted into a predicate function whose only argument
# is the object in question.
Pred = Union[
    Type,
    Tuple,  # should be Tuple[Pred, ...]
    Callable[[Any], bool],
    None
]

def as_pred(o: Pred) -> Callable[[Any], bool]:
    if isclass(o):
        return lambda x: isinstance(x, o)  # type: ignore[arg-type]  # mypy bug?
    elif isinstance(o, tuple):
        preds = tuple(as_pred(p) for p in o)
        return lambda x: any(p(x) for p in preds)
    elif callable(o):
        return o
    elif o is None:
        return lambda x: True
    else:
        return lambda x: match_wo_none(x, o)

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

# An activation level
Activation = float

# A dictionary mapping things to activation levels
ADict = Dict[Hashable, Activation]
