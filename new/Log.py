# Log.py

from __future__ import annotations
from dataclasses import dataclass, replace, field, InitVar
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, IO, \
    runtime_checkable
import csv
import sys
from contextlib import contextmanager

from FMTypes import ADict
from Indenting import Indenting, indent
from util import short


logfile = Indenting(sys.stdout)

class Loggable(ABC):
    '''Mix-in for classes that know how to print their own log entries.'''

    @abstractmethod
    def log(self, fm: FARGModel, f: Indenting) -> None:
        '''Called by the 'logging()' context manager to indicate that self
        should make a log entry. Must print to 'f'.'''
        pass

@contextmanager
def logging(fm: FARGModel, arg: Any):
    try:
        if logging_is_enabled(arg):
            with indent(logfile):
                if isinstance(arg, Loggable):
                    arg.log(fm, logfile)
                else:
                    raise NotImplementedError(arg)
        yield logfile
    finally:
        pass

def logging_is_enabled(arg: Any) -> bool:
    return True
