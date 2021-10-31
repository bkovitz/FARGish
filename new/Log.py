# Log.py

from __future__ import annotations
from dataclasses import dataclass, replace, field, InitVar
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, IO, \
    runtime_checkable
import csv

from FMTypes import ADict
from util import short


@dataclass
class ALogger:
    '''An activation logger.'''
    t: int
    filename: str = 'a.csv'
    subt: int = 0
    f: InitVar[IO] = None
    mode: InitVar[Optional[str]] = None
    writer: InitVar[Any] = None  # csv.writer

    def __post_init__(self, f, mode, writer): #, mode):
        #if not mode:
            #mode = 'w' if self.t == 0 else 'a'
        mode = 'w'
        self.f = open(self.filename, mode=mode, newline='')
        self.writer = csv.writer(self.f, quoting=csv.QUOTE_NONNUMERIC)
        print('t,subt,node,a', file=self.f)

    def write(self, d: ADict) -> None:
        for node, a in d.items():
            self.writer.writerow([self.t, self.subt, short(node), a])  # type: ignore[attr-defined]

    def bump_subt(self) -> None:
        self.subt += 1
