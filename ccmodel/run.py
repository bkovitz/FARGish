# run.py  -- run() and mk_func_args()

from __future__ import annotations
from dataclasses import dataclass, field, replace
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    get_type_hints, runtime_checkable, TYPE_CHECKING
import inspect

from Fizzle import Fizzle, MissingArgument
from ArgsMap import ArgsMap, ArgsMapSeries, as_argsmap
from util import force_setattr, short, as_dstr, is_type_instance
if TYPE_CHECKING:
    from FMTypes import TypeAnnotation
    from Program import Program, ProgramResult


def run(program: Program, args: ArgsMap) -> ProgramResult:
    try:
        return program.run(
            **mk_func_args(program.run, ArgsMapSeries.make(
                args,
                as_argsmap(program)
            ))
        )
    except Fizzle as fiz:
        if fiz.codelet is None:
            force_setattr(fiz, 'codelet', program)
        raise

def mk_func_args(func: Callable, args: ArgsMap) -> Dict[str, Any]:
    d: Dict[str, Any] = {}
    for param_name, param_type in params_of(func):
        value = args.get(param_name, None)
        if not is_type_instance(value, param_type):
            # TODO Distinguish between missing argument and argument of
            # wrong type?
            raise MissingArgument(
                func=func,
                param_name=param_name,
                param_type=param_type,
                value=value,
                actual_type=type(value)
            )
        else:
            d[param_name] = value
    return d

def params_of(func: Callable) -> Iterable[Tuple[str, TypeAnnotation]]:
    type_hints = get_type_hints(func)
    for param_name in inspect.signature(func).parameters:
        if param_name == 'return':
            continue  # disregard return type
        yield (param_name, type_hints.get(param_name, Any))
