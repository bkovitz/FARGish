# Model2.py -- The canvas-and-painters model, rewritten to make every
#              workspace object a dictionary

#The seed of the idea:
#Seed = WObj('letter'=Letter, 'i'=Index) 

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check, get_type_hints, get_args
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod
from itertools import chain
from collections import defaultdict
import re
from pprint import pp

from pyrsistent import pmap
from pyrsistent.typing import PMap

from Log import lo, trace
from util import as_iter, field_names_and_values, first, force_setattr, \
    intersection, pr, safe_issubclass, short, union


@dataclass(frozen=True)
class WObj:
    name: Var
    d: Dict[


@dataclass
class Model:
    ws: Dict[str, WObj]

    def process_A(self) -> None:
        '''Run detectors, which generate painters that reconstruct what the
        detectors detected.'''
        detector = self.choose_detector()
        detector.run(self)

    def process_B(self) -> None:
        '''Run painters, which generate new canvases and new painters.'''
        painter = self.choose_painter()
        painter.run(self)


'''Top-level code

parse the problem definition
    put snippets into workspace, tagged with World and Side relations

loop:
    process A: from values to painters
        run detectors
            maybe the detectors create some new workspace objects
    process B: from painters to values
        choose a painter that hasn't run, and run it

detectors:
    repetition-detector
    differ

painters:
    Succ
    Same
    Pred
    Repeat
    PainterCluster

'''

if __name__ == '__main__':
    s1 = WObj('S1', {Typ: Canvas, Address: A(1), Content: 'abc', Length: 3})
    s2 = WObj('S2', {Typ: Canvas, Address: A(2), Content: 'abd', Length: 3})
    s3 = WObj('S3', {Typ: Canvas, Address: A(3), Content: 'ijk', Length: 3})
    s4 = WObj('S4', {Typ: Canvas, Address: A(4)})

    m = Model()
    m.add(s1)
    m.add(s2)
    m.add(s3)
    m.add(s4)
    # add the World and Side tags

    
