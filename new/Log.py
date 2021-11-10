# Log.py

from __future__ import annotations
from dataclasses import dataclass, replace, field, InitVar
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, IO, \
    runtime_checkable
from abc import ABC, abstractmethod
import csv
import sys
from contextlib import contextmanager
import functools

from FMTypes import ADict, Pred, as_pred
from Indenting import Indenting, indent
from util import short, dict_str, pr, pts


_logfile = Indenting(sys.stdout)
enabled_for_logging: Set[Callable[[Any], bool]] = set()
# TODO Set this up so enabled_for_logging won't get cleared if FARGModel.py
# gets imported after you call lenable().

class Loggable(ABC):
    '''Mix-in for classes that know how to print their own log entries.'''

    @abstractmethod
    def log(self, f: Indenting, **kwargs) -> None:
        '''Called by the 'logging()' context manager to indicate that self
        should make a log entry. Must print to 'f'.'''
        pass

def lo(*args, **kwargs) -> None:
    '''Prints args to log file, at current indentation level.'''
    print(*args, dict_str(kwargs), file=logfile())

def log_to(f: IO) -> None:
    '''Set logfile to f. Wraps f with Indenting.'''
    global _logfile
    _logfile = Indenting(f)

def logfile() -> Indenting:
    global _logfile
    return _logfile

@contextmanager
def logging(lg: Union[Loggable, None], *args, **kwargs):
    # TODO Document how to use this in a 'with' statement.
    try:
        if lg is None:
            lo(*args, **kwargs)
        elif logging_is_enabled(lg):
            lg.log(_logfile, *args, **kwargs)
        with indent(_logfile):
            yield _logfile
    finally:
        pass

def lenable(*args: Pred) -> None:
    '''Enables logging for args.'''
    for arg in args:
        enabled_for_logging.add(as_pred(arg))

def ldisable(*args: Pred) -> None:
    '''Disables logging for args.'''
    for arg in args:
        enabled_for_logging.discard(as_pred(arg))

def ldisable_all() -> None:
    '''Disables all logging.'''
    enabled_for_logging.clear()

def logging_is_enabled(arg: Any) -> bool:
    '''Is logging enabled for arg?'''
    return any(pred(arg) for pred in enabled_for_logging)
    """
    pred = as_pred(arg)
    print('PRED', pred, enabled_for_logging)
    return (
        any(pred(a) for a in enabled_for_logging)
        or
        arg in enabled_for_logging
    )
    """

def trace(func):
    '''Function decorator: prints the name and arguments of the function each
    time it is called, and prints its return value when it returns.
    Caution: 'trace' will read generators all the way to their end.'''
    @functools.wraps(func)
    def wrapper(*args, **kwargs) -> None:
        argstring = ''
        if args:
            argstring += ', '.join(repr(a) for a in args)
        if kwargs:
            if argstring:
                argstring += ', '
            argstring += ', '.join(
                f'{name}={value}' for name, value in kwargs.items()
            )
        with logging(None, f'{func.__name__}({argstring})'):
            result = func(*args, **kwargs)
        lo(f'-> {result}')
        return result
    return wrapper
