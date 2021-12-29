# Fizzle.py

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING

#from Complex import Complex
from util import as_dstr, short, force_setattr, as_iter
if TYPE_CHECKING:
    from Canvas import Cell, CellRef
    from CCTypes import Program
    from FMTypes import TypeAnnotation

@dataclass(frozen=True)
class Fizzle(Exception):
    codelet: Optional[Program] = None  # This should always be a Codelet
        # The global run() sets .codelet so individual Codelets' .run()
        # methods don't have to.

    def paint_tagged_codelet(self, cellref: Union[Cell, CellRef]) -> None:
        if self.codelet is not None:
            # TODO Pass the buck to cellref to add the tag. We might need
            # to pass a FARGModel, too, to hold the Complex.
            #Commented out to prevent circular import
            #cellref.paint(Complex(self.codelet, tags=(self,)))
            pass
        
    def __str__(self) -> str:
        return as_dstr(self)

    def short(self) -> str:
        return str(self)

@dataclass(frozen=True)
class MissingArgument(Fizzle):
    func: Optional[Callable] = None
    param_name: Optional[str] = None
    param_type: TypeAnnotation = None
    value: Any = None
    actual_type: Optional[Type] = None

    def short(self) -> str:
        cl = self.__class__.__name__
        #lo('MAR', type(self.func))
        return f'{cl}({short(self.func)}, {repr(self.param_name)}, {short(self.param_type)}, {short(self.value)}, {short(self.actual_type)})'

    __str__ = short
