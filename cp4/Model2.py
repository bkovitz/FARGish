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


Address = Any
AttrType = Any
AttrValue = Any

@dataclass(frozen=True)
class WObj:
    name: Var
    attrd: Dict[AttrType, AttrValue]  # intrinsic attributes
    argd: Dict[Var, Value]            # arguments, like where to paint

# {Length: 3, Content: 'abc'}
# {Length: 'L1', Content: 'abc'}

class Detector:

    @classmethod
    @abstractmethod
    def examine_model(cls, m: Model) -> Iterable[Painter]:
        pass

    @classmethod
    def run(cls, m: Model):
        for painter in cls.examine_model(m):
            m.add(painter)

    @classmethod
    @abstractmethod
    def generate_pairs(self, m: Model, addr: Address) \
    -> Iterable[Tuple[Address, Address]]:
        pass

class Succ(Detector):

    @classmethod
    def examine_pair(cls, m: Model, la: Address, ra: Address) \
    -> Iterable[Painter]:
        '''Convention: la is always to the left of ra.'''
        match m.at(left), m.at(right):
            case (Letter(left), Letter(right)):
                if r.is_succ_of(l):
                    yield WObj.make(Succ, la, ra)

    @classmethod
    def run(cls, m: Model, me: WObj) -> None:
        la = me.get_address('AA1')  # parameter name -> argument
        ra = me.get_address('AA2')
        match (m.at(la), m.at(ra)):
            case (Letter(l), Blank()):
                m.paint(ra, l.succ())
            case (Blank(), Letter(r)):
                m.paint(la, r.pred())
            case (Letter(l), Letter(r)):
                if not r.is_succ_of(l):
                    raise FailedMatch

@dataclass
class Model:
    ws: Dict[str, WObj]

    def process_A(self) -> None:
        '''Run detectors, which generate painters that reconstruct what the
        detectors detected.'''
        detector = self.choose_detector()
        for painter in detector.run(self):
            self.define_and_name(painter)

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

    
