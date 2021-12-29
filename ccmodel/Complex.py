# Complex.py

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING

from CCTypes import Program, HasHasTag
from FMTypes import Value
from Tag import tagmatch
from run import run
from util import force_setattr, as_tuple, short, as_iter
if TYPE_CHECKING:
    from ArgsMap import ArgsMap, ArgsDict, ArgsMapSeries
    from Program import ProgramResult
    from FMTypes import Pred


@dataclass(frozen=True)
class Complex(Program, HasHasTag):
    # TODO Replace .nugget and .override with a single dict? This would make
    # it straightforward to override the nugget.
    nugget: Complex
    override: ArgsDict
    tags: Optional[Tuple[Value]]

    def __init__(
        self,
        nugget: Union[Program, Complex],
        *args: Union[Dict[str, Value], ArgsMap],
        tags: Optional[Sequence[Value]]=None,
        **kwargs
    ):
        force_setattr(self, 'nugget', nugget)
        force_setattr(self, 'override', ArgsMapSeries.make(
            *args,
            ArgsDict(kwargs)
        ))
        if tags:
            force_setattr(self, 'tags', as_tuple(tags))
        else:
            force_setattr(self, 'tags', None)
        #lo('COMPL', self.tags)

    def run(self, args: Optional[ArgsMap]=None) -> ProgramResult:  # type: ignore[override]
        return run(self.nugget, ArgsMapSeries.make(
            args, self.override
        ))

    def has_tag(self, pred: Pred) -> bool:
        return any(c._has_tag(pred) for c in self.complexes_all_down())

    def _has_tag(self, pred: Pred) -> bool:
        #return any(isinstance(t, tag) for t in as_iter(self.tags))
        return any(tagmatch(t, pred) for t in as_iter(self.tags))

    def complexes_all_down(self) -> Iterable[Complex]:
        x = self
        while isinstance(x, Complex):
            yield x
            if not isinstance(x.nugget, Complex):
                break
            x = x.nugget

    def short(self) -> str:
        return f'{short(self.override)}/{short(self.nugget)}'

