# Features.py -- Classes for Feature nodes

from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, IO, \
    runtime_checkable

from FMTypes import Node
from util import isclass


@dataclass(frozen=True)
class Feature:
    pass

Features = Union[None, Feature, Sequence[Feature]]

def features_of(x: Any) -> Iterable[Node]:
    if not isclass(x):
        if hasattr(x, 'features_of'):
            yield from x.features_of()
        yield type(x)  # type: ignore[misc]  # Type isn't Hashable??

# TODO rm?
@dataclass(frozen=True)
class FeatureWrapper(Feature):
    feature: Union[Hashable, None] = None

# Generic features

@dataclass(frozen=True)
class Before(Feature):
    x: Node

    def features_of(self) -> Iterable[Node]:
        yield self.x

@dataclass(frozen=True)
class After(Feature):
    x: Node

    def features_of(self) -> Iterable[Node]:
        yield self.x

# Numerical, or related to Equations

@dataclass(frozen=True)
class IncreaseOrDecrease(Feature):
    name: str

    def __str__(self):
        return self.name

Increase = IncreaseOrDecrease('Increase')
Decrease = IncreaseOrDecrease('Decrease')

@dataclass(frozen=True)
class NumOperands(Feature):
    num: int

    def features_of(self):
        yield self.num

@dataclass(frozen=True)
class MinBefore(Feature):
    x: Node

    def features_of(self):
        yield self.x

@dataclass(frozen=True)
class MaxBefore(Feature):
    x: Node

    def features_of(self):
        yield self.x

@dataclass(frozen=True)
class Doubled(Feature):
    x: Node

    def features_of(self):
        yield self.x
