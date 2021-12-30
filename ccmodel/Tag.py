# Tag.py -- Base classes for tags, in CCModel.py

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from abc import ABC, abstractmethod
from inspect import isclass, signature

from FMTypes import Pred, as_pred, N, T, Value
from CCTypes import HasWithTag, HasAddTag, HasHasTag, Codelet
from Fizzle import Fizzle
from Log import trace, lo
from run import mk_func_args
from ArgsMap import ArgsMap, ArgsMapWithTags, ArgsDict, EmptyArgsMap, \
    empty_args_map, ArgsMapSeries, as_argsmap, Avails
from util import short, force_setattr
from Canvas import NodeRef
if TYPE_CHECKING:
    from CCModel import Codelet
    from FARGBase import FARGModel
    from Program import ProgramResult


def is_cell_tag_pred(pred: Pred) -> bool:
    '''Does 'pred' test for a CellTag?'''
    return (
        isinstance(pred, CellTag)
        or
        (isclass(pred) and issubclass(pred, CellTag))  # type: ignore[arg-type]
    )

def has_tag(x: Any, pred: Pred) -> bool:
    if isinstance(x, HasHasTag):
        return x.has_tag(pred)
#    elif isinstance(x, (Complex, Cell, CellRef)):  # TODO rm
#        return x.has_tag(tag)
    else:
        return False

def tagmatch(tag: Any, pred: Pred) -> bool:
    if isinstance(tag, PTag) and not isinstance(pred, PTag):
        return as_pred(pred)(tag.tagpred)
    else:
        return as_pred(pred)(tag)

@dataclass(frozen=True)
class HasTag:
    '''A predicate to check if an object has a given tag or type of tag, but
    not to check if the object has the TagPred for that tag.'''
    pred: Pred

    def __call__(self, noderef: NodeRef) -> bool:
        return has_tag(noderef, self.pred)

    @classmethod
    def make_from(cls, tagspec: TagSpec) -> HasTag:
        if isinstance(tagspec, TagPred):
            return HasTag(PTag(tagspec))
        else:
            return HasTag(tagspec)

class TagPred:
    '''A predicate for determining if a node (or some nodes, or anything)
    meets the criteria for a tag. Derive tag classes from TagPred.
    Instances of those tag classes (possibly including arguments) go inside
    Tag objects.'''

    @abstractmethod
    def condition(self, **kwargs) -> bool:
        pass

    def run(self, args: ArgsMap=empty_args_map, **kwargs) -> bool:
        return self.condition(**mk_func_args(
            self.condition, ArgsMapSeries.make(
                ArgsDict(kwargs),
                args,
                as_argsmap(self)
            ))
        )

@dataclass(frozen=True)
class PTag:
    '''A Tag that means that its taggee satisfies a certain TagPred.'''
    tagpred: TagPred

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.tagpred)})'

@dataclass(frozen=True)
class Tagger(Codelet):
    tagpred: TagPred

    def run(self, fm: FARGModel) -> ProgramResult:  # type: ignore[override]
        pass  # TODO Look for something to tag, and call .run_on() on it.

    def run_on(
        self,
        fm: FARGModel,
        noderef: NodeRef,
        args: ArgsMap=empty_args_map
    ) -> None:
        if self.tagpred.run(args, fm=fm, noderef=noderef):
            fm.add_tag(noderef, PTag(self.tagpred))

@dataclass(frozen=True)
class HasAvail(TagPred):
    target: Optional[Value] = None

    def condition(self, noderef: NodeRef, target: Value) -> bool:  # type: ignore[override]
        av = as_argsmap(noderef).get('avails')
        return isinstance(av, Avails) and av.has_avail(target)

class SimpleTag:
    '''A tag that doesn't have a predicate.'''
    pass

class CellTag:
    '''A tag that goes on a Cell rather than CellContents.'''
    pass

#TagConjunct = Union[SimpleTag, Fizzle, TagPred]
Tag = Union[SimpleTag, PTag, Fizzle]
TagSpec = Union[Tag, TagPred, Type[Tag], Type[TagPred]]

@dataclass(frozen=True)
class TagConjunction(TagPred):
    '''A Tag predicate (tester) for multiple tags at once.'''
    conjuncts: Tuple[Pred, ...]
    #scope: SeqCanvas  # TODO Allow other scopes, like the whole workspace

    def __init__(self, conjuncts_: Tuple[TagSpec, ...]):
        force_setattr(
            self, 'conjuncts', tuple(HasTag.make_from(c) for c in conjuncts_)
        )

    def condition(self, fm: FARGModel, conjuncts: Tuple[Pred]) -> bool:  # type: ignore[override]
        """
        for noderef in self.scope:
            # How do we decide whether to call has_tag or cell_has_tag?
            # How do we get the predicate "HasTag" from the conjunct?
            pass #TODO
        """
            
        # WANT Find the node or cell that has all the conjuncts
        return False # TODO
