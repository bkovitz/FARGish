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

from Pred import Pred, as_pred
from Indenting import Indenting, indent
from util import short, dict_str, pr, pts


_logfile = Indenting(sys.stdout)
log_level: int = 0  # Log level 0 means: don't log anything

enabled_for_logging: Set[Callable[[Any], bool]] = set()
# TODO Set this up so enabled_for_logging won't get cleared if FARGModel.py
# gets imported after you call lenable().

def set_log_level(n: int) -> None:
    global log_level
    log_level = n

def lo(*args, **kwargs) -> None:
    '''Prints args to log file, at current indentation level.'''
    global log_level
    if len(args) >= 1 and isinstance(args[0], int):
        if log_level > args[0]:
            return
        else:
            args = args[1:]
    print(
        *(short(a) for a in args),
        dict_str(kwargs, xform=short),
        file=logfile()
    )

def log_to(f: IO) -> None:
    '''Set logfile to f. Wraps f with Indenting.'''
    global _logfile
    _logfile = Indenting(f)

def logfile() -> Indenting:
    global _logfile
    return _logfile

@contextmanager
def indent_log(*args, **kwargs):
    try:
        lo(*args, **kwargs)
        with indent(_logfile):
            yield _logfile
    finally:
        pass

def trace(func):
    '''Function decorator: prints the name and arguments of the function each
    time it is called, and prints its return value when it returns.
    Caution: 'trace' will read generators all the way to their end.'''
    @functools.wraps(func)
    def wrapper(*args, **kwargs) -> None:
        argstring = ''
        if args:
            #argstring += ', '.join(repr(a) for a in args)
            argstring += ', '.join(short(a) for a in args)
        if kwargs:
            if argstring:
                argstring += ', '
            argstring += ', '.join(
                f'{name}={value}' for name, value in kwargs.items()
            )
        with logging(None, f'{func.__name__}({argstring})'):
            result = func(*args, **kwargs)
        lo(f'-> {short(result)}')
        return result
    return wrapper
