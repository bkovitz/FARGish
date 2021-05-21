# FARGish2.py -- The generic classes and functions for making a FARG model

# Unlike in FARGish.py, this version does not represent sequences with a Cell
# for each step (containing multiple, competing Values). Instead, every
# distinct temporal sequence is represented by a separate object.


# Consume: provide arguments; get them from a source


from pprint import pprint as pp
import inspect
from time import process_time

from dataclasses import dataclass, field, replace
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence
from numbers import Number, Real
from math import exp
from abc import ABC, abstractmethod
from itertools import chain
from copy import copy
import operator
from operator import itemgetter, attrgetter
from heapq import nlargest
from collections import Counter
from io import StringIO
from inspect import isclass

import networkx as nx
import matplotlib.pyplot as plt
#import netgraph

from Propagator import Propagator, Delta
from util import is_iter, as_iter, as_list, pts, pl, csep, ssep, as_hashable, \
    backslash, singleton, first, tupdict, as_dict, short


# Types

Value = NewType('Value', Hashable)
Addr = NewType('Addr', Hashable)

class Elem(ABC):
    '''Anything that can go in the ws without being contained by something
    else; not a CellRef and not a Value (except for special CellRefs and
    Values that have been made to serve as Elems).'''

    '''
    @abstractmethod
    def search(self, fm: 'FARGModel', pred: Union[Type, Callable]) \
    -> Iterable[Union['Elem', 'CellRef']]:
        pass
    '''

Searchable1 = Union[Elem, 'CellRef']
Searchable = Union[Searchable1, Iterable[Searchable1]]

class Agent(Elem):

    @abstractmethod
    def go(self, fm: 'FARGModel', **overrides):
        '''The Agent should do its agenda tentatively, i.e. without painting
        on any real canvases.'''
        pass

    @abstractmethod
    def act(self, fm: 'FARGModel', **overrides):
        '''The Agent should do agenda officially, including painting values on
        real canvases.'''
        pass

# Generic functions with defaults

def name_of(o: Any):
    try:
        return o.__name__
    except AttributeError:
        return short(o)

def has_avail_value(
    elem: Union['CellRef', Elem, None],
    v: Value
) -> Union[bool, None, 'CellRef', Elem]:
    if elem is None:
        return False
    try:
        return elem.has_avail_value(v)
    except AttributeError:
        if elem == v:
            return elem
        else:
            return False

def search(
    fm: 'FARGModel',
    items: Searchable,
    pred: Union[Type, Callable]
) -> Iterable[Union['Elem', 'CellRef']]:
    for item in as_iter(items):
        if hasattr(item, 'search'):
            yield from item.search(fm, pred)
        else:
            # TODO Figure out the correct response here
            print('NO-SEARCH', item)
            raise NotImplementedError

def search1(*args, **kwargs):
    # TODO Give higher probability based on activation
    return first(search(*args, **kwargs))

# Exceptions

@dataclass(frozen=True)
class Halt(Exception):
    pass

@dataclass(frozen=True)
class ValueNotAvail(Exception):
    container: Any
    value: Any

    def try_to_fix(
        self, fm: 'FARGModel', behalf_of: 'Agent', builder=Union[Agent, None]
    ):
        # TODO The decision of how to go about fixing the problem should
        # result from a slipnet search rather than being hard-coded.
        fm.build(
            Detector(self.value, MakeAgentSeq(tail=behalf_of)),
            builder=builder
        )

# Classes

@dataclass
class ElemInWS:
    '''An element in the workspace.'''
    elem: Elem
    builder: Union[Agent, None] = None
        # TODO Allow multiple builders?
    # activation: float = 1.0

    def __str__(self):
        return f'{self.elem}  builder={self.builder}'

@dataclass
class FARGModel:
    #ws: Set[Hashable] = field(default_factory=set, init=False)
    ws: Dict[Hashable, ElemInWS] = field(default_factory=dict)
    t: int = 0

    # TODO support_g and associated methods
    # TODO activation_g and associated methods

    #def paint_value(self, canvas: 'Canvas', addr: Addr, v: Value):
    def paint_value(
        self, dest: 'CellRef', v: Value, builder: Union[Agent, None]=None
    ) -> 'CellRef':
        cr = dest.paint_value(self, v, builder=builder)
        print(f'PAINTED {v} in {cr}')
        return cr

    # Codelet functions

    def build(self, obj, builder: Union[Agent, None]=None) -> Hashable:
        # TODO Support graph, activation graph, initialize activation
        o = self.in_ws(obj)
        print('BU', obj, o)
        if o is not None:  # If somegthing == obj is already in ws
            return o
        else:
            self.ws[obj] = ElemInWS(obj, builder)
            print('BUI', obj)
            return obj
#        for o in self.ws.intersection(frozenset([obj])):
#            return o
#        else:
#            self.ws.add(obj)
#            return obj

    def search_ws1(self, pred: Union[Type, Callable]) \
    -> Union[Elem, bool, 'CellRef']:
        # TODO Search randomly; weight by activation
        for elem in self.ws.keys():
            found = search1(self, elem, pred)
            if found:
                return found
        return False

    # Ancillary functions, callable by codelets and Agents

    def in_ws(self, obj: Hashable) -> Hashable:
        '''Returns the object from the workspace if it exists, otherwise
        None.'''
        try:
            eiws = self.ws[obj]
        except KeyError:
            return None
        return eiws.elem

    # Functions for display and debugging

    def ws_query(self, pred: Union[Type, Callable], **kwargs) \
    -> Iterable[Hashable]:
        builder = kwargs.pop('builder', None)
        if isclass(pred):
            cl = copy(pred)
            pred = lambda _, x: isinstance(x, cl)
        for eiws in list(self.ws.values()):
            if (
                pred(self, eiws.elem)
                and
                (builder is None or eiws.builder == builder)
                and
                self.argsmatch(kwargs, eiws.elem)
            ):
                yield eiws.elem

    def ws_query1(self, pred: Union[Type, Callable], **kwargs) -> Hashable:
        return first(self.ws_query(pred, **kwargs))
        
    @classmethod
    def argsmatch(cls, kwargs: Dict, elem) -> bool:
        try:
            return all(
                getattr(elem, k) == v
                    for k, v in kwargs.items()
            )
        except AttributeError:
            return False

    def __str__(self):
        result = StringIO()
        print(f't={self.t}', file=result)
        for s in sorted(str(item) for item in self.ws.values()):
            # TODO Include activation level
            print(f'  {s}', file=result)
        return result.getvalue()

    def pr(self, pred: Union[Type, Callable, None], **kwargs) -> Hashable:
        '''Print a subset of the workspace.'''
        print(f't={self.t}')
        for s in sorted(str(item) for item in self.ws_query(pred, **kwargs)):
            print(f'  {s}')

@dataclass(frozen=True)
class StateDelta:
    '''A change from one state to another.'''
    before: Any    # What got replaced
    after: Any     # What replaced it
    how: Union[Any, None]   # Some clue about how it happened

    def seq_str(self):
        '''How to display this inside SeqState.__str__.'''
        return str(self)

@dataclass(frozen=True)
class SeqState:  # TODO Inherit from Value?
    avails: Union[Tuple[int], None] = None
    last_move: Union[StateDelta, None] = None

    #TODO In __iter__, make a tuple out of avails if it's not Hashable
    def take_avails(self, values: Iterable[Value]) \
    -> Tuple[Iterable[Value], Iterable[Value]]:
        '''Returns (taken_avails, remaining_avails). Might raise
        ValueNotAvail.'''
        remaining_avails = [] if self.avails is None else list(self.avails)
        taken_avails = []
        for v in values:
            try:
                remaining_avails.remove(v)
            except ValueError:
                raise ValueNotAvail(self, v)
            taken_avails.append(v)
        return (taken_avails, remaining_avails)

    def has_avail_value(self, v: Value) -> bool:
        return v in self.avails

    def last_move_str(self):
        try:
            return self.last_move.seq_str()
        except AttributeError:
            return str(self.last_move)

    def __str__(self):
        if self.avails is None:
            avails_str = str(self.avails)
        else:
            avails_str = f"({' '.join(str(a) for a in self.avails)})"
        if self.last_move is None:
            return avails_str
        else:
            return f'{self.last_move_str()} {avails_str}'


@dataclass(frozen=True)  # TODO Allow AgentSeq to change
class AgentSeq(Agent):
    agents: Tuple[Agent]
    initial_kwargs: Collection[Tuple[str, Hashable]]

    def go(self, fm: FARGModel, **overrides):
        kwargs = {**as_dict(self.initial_kwargs), **overrides}
        source = kwargs.get('source', None)

        for agent in self.agents:
            agent = replace(agent, **kwargs)
            dest = source.imaginary_next_cellref()
            source = agent.go(fm, source=source, dest=dest, builder=self)
            #kwargs = self.next_kwargs(kwargs)
        # TODO Return a value

    def act(self, fm: FARGModel, **overrides):
        kwargs = {**as_dict(self.initial_kwargs), **overrides}
        source = kwargs.get('source', None)

        for agent in self.agents:
            agent = replace(agent, **kwargs)
            dest = source.next()
            source = agent.act(fm, source=source, dest=dest, builder=self)
        # TODO Return a value

    '''
    def next_kwargs(self, kwargs: Dict[str, Hashable]) -> Dict:
        # TODO This method needs to be specified as an arg itself somehow
        result = copy(kwargs)
        dest = result['dest']
        result['source'] = dest
        result['dest'] = dest.next()
        return result
    '''

    def __str__(self):
        cl = self.__class__.__name__
        ags = ', '.join(str(a) for a in self.agents)
        return f'{cl}({ags})'

@dataclass(frozen=True)
class MakeAgentSeq:
    tail: Agent  # TODO Allow an AgentSeq, too

    def __call__(self, fm: FARGModel):
        # TODO
        pass

    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}(tail={self.tail})'

@dataclass(frozen=True)
class HasAvailValue:
    v: Value

    def __call__(self, fm: FARGModel, elem) -> bool:
        return has_avail_value(elem, self.v)

@dataclass(eq=False)
class Canvas(Elem, ABC):

    @abstractmethod
    def raw_paint_value(self, fm: FARGModel, addr: Addr, v: Value):
        '''Put Value v at address addr. The lowest-level canvas-writing
        operation.'''
        pass

    # TODO Make this into __getitem__?
    @abstractmethod
    def get_value(self, fm: FARGModel, addr: Addr) -> Union[Value, None]:
        '''Returns the value at addr, or None if the cell at addr is blank.'''
        pass

    @abstractmethod
    def __getitem__(self, addr: Addr) -> Value:
        pass

    @abstractmethod
    def cellrefs(self) -> Iterable['CellRef']:
        pass

@dataclass(eq=False)
class SeqCanvas(Canvas):
    states: List[SeqState] = field(default_factory=list)

    def raw_paint_value(self, fm: FARGModel, addr: Addr, v: Value):
        # TODO Handle addr that doesn't work as a list index
        # TODO Accept a builder argument?
        while len(self.states) <= addr:
            self.states.append(None)
        self.states[addr] = v

    def get_value(self, fm: FARGModel, addr: Addr) -> Union[Value, None]:
        # TODO Handle addr that can't be found or is not an index
        return self.states[addr]

    def __getitem__(self, addr: Addr) -> Value:
        # TODO Handle addr that can't be found or is not an index
        print('SEQCGET', addr, len(self.states))
        return self.states[addr]

    def search(self, fm, pred):
        yield from search(fm, self.cellrefs(), pred)

    def cellrefs(self):
        for addr, state in enumerate(self.states):
            yield CellRef(self, addr)
        
    def has_avail_value(self, v: Value):
        for addr, state in enumerate(self.states):
            if has_avail_value(state, v):
                return CellRef(self, addr)
        return False

    def as_solution(self) -> str:
        return '; '.join(
            st.last_move_str() for st in self.states if st.last_move
        )

    def __str__(self):
        return f"SeqCanvas({'; '.join(str(st) for st in self.states)})"

@dataclass(frozen=True)
class CellRef:
    canvas: Union[SeqCanvas, None] = None
    addr: Union[int, None] = None

    @property
    def contents(self) -> Hashable:
        # TODO What if .canvas is not in FARGModel?
        return self.canvas[self.addr]

    def paint_value(
        self, fm: FARGModel, v: Value, builder: Union[Agent, None]=None
    ) -> 'CellRef':
        # TODO Should we do something with builder?
        self.canvas.raw_paint_value(fm, self.addr, v)
        return self

    def has_avail_value(self, v):
        return has_avail_value(self.contents, v)

    def preceding_contents(self) -> Hashable:
        if self.addr <= 0:
            return self.contents
        else:
            return self.canvas[self.addr - 1]

    def next(self) -> 'CellRef':
        return replace(self, addr=self.addr + 1)

    def is_real(self):
        return True

    def imaginary_next_cellref(self):
        '''Returns an ImCell to hold a hypothetical/imagined next cell after
        this one.'''
        return ImCell(canvas=self.canvas, addr=self.addr + 1)

    def __str__(self):
        cl = self.__class__.__name__
        return f'canvas[{self.addr}]'

@dataclass(frozen=True)
class ImCell(CellRef):
    '''An imaginary cell, not in a canvas. The .contents is a Value that Agents
    are considering constructing or treating hypothetically as existing.'''
    contents: Value = None    # None means empty; empty ImCells are not allowed
                              # in the workspace.

    def paint_value(
        self, fm: FARGModel, v: Value, builder: Union[Agent, None]=None
    ) -> CellRef:
        '''Builds ImCell(v) if v is different than .contents.'''
        print('ImC', v, self.contents)
        if v != self.contents:
            print('ImC2')
            return fm.build(replace(self, contents=v), builder=builder)
        else:
            return self
        
    def is_real(self):
        return False

    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.contents})'
    

@dataclass(frozen=True)
class Painter(ABC):

    @abstractmethod
    # TODO Do we really need to pass 'fm'?
    def paint(self, fm: FARGModel):
        pass

@dataclass(frozen=True)
class LiteralPainter(Painter):
    cell: CellRef
    value: Value

    def paint(self, fm: FARGModel):
        # TODO Throw exc if any members are missing? Or should that be an
        # assertion (if it's illegal to create a LiteralPainter that's missing
        # any args)?
        #self.canvas.paint_value(fm, self.addr, self.value)
        self.cell.paint_value(fm, self.value)

    def has_avail_value(self, v: Value) -> bool:
        return has_avail_value(self.value, v)

    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.cell}, {self.value})'

@dataclass(frozen=True)
class Blocked:
    taggee: Hashable
    reason: Hashable

    def go(self, fm: FARGModel):
        # TODO The .reason might not have a .try_to_fix method (and probably
        # shouldn't).
        self.reason.try_to_fix(fm, behalf_of=self.taggee, builder=self)

    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.taggee}, {self.reason})'
    
@dataclass(frozen=True)
class Detector:
    target: Value  # Change to a match function?
    action: Callable

    def go(self, fm: FARGModel):
        # TODO See if self.target is there, favoring new elems

        #found = fm.ws_query(pred=HasAvailValue(self.target))
        found = CellWithAvailValue(self.target).search(fm)
        found = list(found)
        #print('FOUND', found)
        for cellref in found:
            if cellref.is_real():
                self.action(fm, cellref)
            #self.action(fm, found, agent=self)

    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.target}, {name_of(self.action)})'
        

def halt(fm: FARGModel, *args, **kwargs):
    raise Halt(*args, **kwargs)

@dataclass(frozen=True)
class RaiseException:
    exc_class: Type

    def __call__(self, fm: FARGModel, *args, **kwargs):
        raise self.exc_class(*args, **kwargs)

@dataclass
class Parg(Painter):
    '''A complex of painter objects and objects specifying arguments for
    them, suitable for support relationships between PargComplexes, painters,
    arguments, and the sources thereof.'''
    painter: Painter
    kwargs: Dict[str, Any] = field(default_factory=dict)

    def add_arg(self, **kwargs):
        '''Alter .painter and .kwargs to match kwargs.'''
        # TODO Update support_g?
        # TODO If an arg already has a non-None value defined, do we create
        # a new Parg with mutual antipathy with the old Parg?
        self.kwargs.update(kwargs)
        self.painter = self.painter.__class__(**self.kwargs)

    def paint(self, fm: FARGModel):
        # TODO Throw exception of .painter doesn't have enough args
        self.painter.paint(fm)

@dataclass(frozen=True)
class MatchFunc(ABC):
    '''Give a MatchFunc a thing and a context, and it will give you a
    number from 0.0 to 1.0. Each MatchFunc specifies how sensitive it
    is to different kinds of things, relative to their context.'''

    @abstractmethod
    def __call__(self, thing, context) -> float:
        pass

def x_in_context(thing, context) -> Real:
    # TODO Consider the context; force thing to be a number if it's not.
    return thing

@dataclass(frozen=True)
class MatchByPeaks(MatchFunc):
    '''A MatchByPeaks is a function with one or more centers of peak
    sensitivity, and a specificity parameter determining how sensitivity falls
    off depending on how far away a stimulus is from a peak. The greater the
    specificity, the faster the function decays as x gets further from a peak.
    The result of a MatchByPeaks is the maximum of the results of trying
    the thing against each peak.'''
    # TODO Add noise

    peaks: Set[Number]
    squeeze: Real = 1.0

    def __call__(self, thing, context) -> float:
        x = x_in_context(thing, context)
        return max(
            (self.peak_f(peak, x) for peak in self.peaks),
            default=0.0
        )

    def peak_f(self, peak, x) -> float:
        return exp(- self.squeeze * (peak - x) **2)

@dataclass(frozen=True)
class ExactMatchFunc(MatchFunc):
    target: Hashable

    def __call__(self, thing, context) -> float:
        if thing == self.target:
            return 1.0
        else:
            return 0.0

@dataclass(frozen=True)
class CellWithAvailValue:
    v: Value

    def is_candidate(self, fm: FARGModel, x: Any) -> bool:
        '''Is x even something we want to check for a match?'''
        return isinstance(x, (CellRef, Canvas))

    def candidates(self, fm: FARGModel) -> Iterable[CellRef]:
        for x in fm.ws_query(self.is_candidate):
            if isinstance(x, Canvas):
                yield from x.cellrefs()
            else:
                yield x

    def search(self, fm: FARGModel) -> Iterable[CellRef]:
        for c in self.candidates(fm):
            if has_avail_value(c, self.v):
                yield c
        
    #def matchpct(self, 

if __name__ == '__main__':
    if False:
        # TODO Make a UT out of this
        fm = FARGModel()
        s0 = SeqState((4, 5, 6), None)
        c = SeqCanvas([s0])
        cu = Consume(plus, (4, 5), c, 1)
        cu.paint(fm)
        print(c)

    if False:
        # TODO Make a UT out of this
        fm = FARGModel()
        s0 = SeqState((4, 5, 6), None)
        c = SeqCanvas([s0])
        p = Parg(Consume())
        p.add_arg(canvas=c)
        p.add_arg(addr=1, operator=plus, operands=(5, 4))
        p.paint(fm)
        print(c)

    if False:
        fm = FARGModel()
        c = SeqCanvas()
        s0 = SeqState((4, 5, 6), None)
        lp = LiteralPainter(c, 0, s0)
        lp.paint(fm)
        print(c)

    if False:  # sketching
        fm = FARGModel()
        ca = fm.add(SeqCanvas([SeqState((4, 5, 6), None)]))
        co = fm.add(Consume(plus, (4, 5)))  # missing canvas and addr
        # make a Consume
        # let it build LiteralPainters
        # LiteralPainters paint on SeqCanvas

    if False:
        mf = MatchByPeaks({4, 7}, 2.0)
        ef = ExactMatchFunc(4)
        for x in range(1, 11):
            print(x, mf(x, None), ef(x, None))


    #s0 = fm.add(Top, SeqState((2, 3, 4, 5, 11), None))
    #g0 = fm.make_seqglom(s0)
    #s1 = fm.consume(s0, Times, (5, 11))
    #g1 = fm.append(g0, s1)
    #s2 = fm.consume(s0, Times, (4, 11))
    #g2 = fm.append(g0, s2)
    #? g2 = fm.consume(g0, Times, (4, 11))
    # Now see the support graph

    # Pons asinorum, hard-coded codelet sequence
    if True:
        fm = FARGModel()
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        wa = fm.build(Want(15, canvas=ca, addr=0))
        wa.go(fm) # Builds Consume objects and Detector

        for co in fm.ws_query(Consume, builder=wa):
            co.go(fm)

        bl = fm.ws_query1(Blocked)
        bl.go(fm)

        d9 = fm.ws_query1(Detector, target=9)
        d9.go(fm)

        co1 = fm.ws_query1(Consume, operands=(4, 5))
        co2 = fm.ws_query1(Consume, operands=(9, 6))
        aseq0 = fm.build(
            AgentSeq(
                (co1, co2),
                initial_kwargs=tupdict(
                    source=CellRef(ca, 0),
                    dest=CellRef(ca, 1)
                )
            )
        )
        print(f'aseq0: {aseq0}')
        aseq0.go(fm)  #This should not complain

        aseq = fm.ws_query1(AgentSeq)
        aseq.act(fm)

        d15 = fm.ws_query1(Detector, target=15)
        try:
            d15.go(fm)
            print('FAILED! Did not detect 15.')
        except Halt as exc:
            print('SUCCEEDED', exc)

        print(fm)
        print()
        pred = CellWithAvailValue(15)
        l = (list(pred.search(fm)))
        pl(l)
        print(len(l))
