# run.py  -- run() and mk_func_args()

from __future__ import annotations
from dataclasses import dataclass, field, replace
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    get_type_hints, runtime_checkable, TYPE_CHECKING
import inspect

from FMTypes import TypeAnnotation
from Fizzle import Fizzle, MissingArgument
from ArgsMap import ArgsMap, ArgsMapSeries, as_argsmap, Avails
from Program import Program, ProgramResult
from Log import lo, trace
from util import force_setattr, short, as_dstr, is_type_instance


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
    # NEXT It looks like get_type_hints() needs *all* the types in the
    # global namespace. I might have to move params_of(), or a special
    # get_type_hints(), to its own file, which imports *everything*. Yuck!
    # func.__annotations__ holds strings rather than types if func was
    # imported.
    try:
        type_hints = get_type_hints(func, globalns=func.__globals__, localns=globals()) # type: ignore[attr-defined]
    except NameError:
        lo('PARAMS_OF', func, func.__annotations__)
        raise
    """
    lo('PARAMS_OF', func, func.__annotations__)
    type_hints = func.__annotations__
    """
    for param_name in inspect.signature(func).parameters:
        if param_name == 'return':
            continue  # disregard return type
        yield (param_name, type_hints.get(param_name, Any))
