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

# TODO Make Agent into a base class
Agent = NewType('Agent', Hashable)


# Generic functions with defaults

def name_of(o: Any):
    try:
        return o.__name__
    except AttributeError:
        return short(o)

def has_avail_value(elem, v: Value) -> bool:
    try:
        return elem.has_avail_value(v)
    except AttributeError:
        return elem == v

# Exceptions

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
    elem: Hashable
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

    def in_ws(self, obj: Hashable) -> Hashable:
        '''Returns the object from the workspace if it exists, otherwise
        None.'''
        try:
            eiws = self.ws[obj]
        except KeyError:
            return None
        return eiws.elem

    def OLDws_query(self, cl: Type, builder=Union[Agent, None]) \
    -> Iterable[Hashable]:
        for eiws in list(self.ws.values()):
            #print('WSQ', eiws, builder, eiws.builder == builder)
            #print('WSQ', cl, isinstance(eiws.elem, cl))
            if isinstance(eiws.elem, cl):
                if builder is None or eiws.builder == builder:
                    yield eiws.elem

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

    # Functions for display and debugging

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

    def __str__(self):
        if self.avails is None:
            avails_str = str(self.avails)
        else:
            avails_str = f"({' '.join(str(a) for a in self.avails)})"
        if self.last_move is None:
            return avails_str
        else:
            try:
                last_move_str = self.last_move.seq_str()
            except AttributeError:
                last_move_str = str(self.last_move)
            return f'{last_move_str} {avails_str}'


@dataclass(frozen=True)  # TODO Allow AgentSeq to change
class AgentSeq:
    agents: Tuple[Agent]
    initial_kwargs: Collection[Tuple[str, Hashable]]

    def go(self, fm: FARGModel):
        kwargs = as_dict(self.initial_kwargs)
        source = kwargs.get('source', None)

        for agent in self.agents:
            agent = replace(agent, **kwargs)
            dest = source.imaginary_next_cellref()
            source = agent.go(fm, source=source, dest=dest, builder=self)
            print(f'NEWSRC = {source}')
            #kwargs = self.next_kwargs(kwargs)

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
class Canvas(ABC):

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

@dataclass(eq=False)
class SeqCanvas:
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

    def preceding_contents(self) -> Hashable:
        if self.addr <= 0:
            return self.contents
        else:
            return self.canvas[self.addr - 1]

    def next(self) -> 'CellRef':
        return replace(self, addr=self.addr + 1)

    def imaginary_next_cellref(self):
        '''Returns an ImCell to hold a hypothetical/imagined next cell after
        this one.'''
        return ImCell()

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
            return fm.build(ImCell(contents=v), builder=builder)
        else:
            return self
        
    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.contents})'
    
@dataclass(frozen=True)
class Operator:
    func: Callable
    name: str

    def call(self, *operands: int) -> int:
        return self.func(*operands)

    def __str__(self):
        return self.name

plus = Operator(operator.add, '+')
times = Operator(operator.mul, 'x')
minus = Operator(operator.sub, '-')

@dataclass(frozen=True)
class ArithDelta(StateDelta):
    '''A completed arithmetic operation.'''
    before: Sequence
    after: Union[Value, Collection]
    how: Operator

    def seq_str(self):
        expr = f' {self.how} '.join(str(n) for n in self.before)
        return f'{expr} = {self.after}'


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
class Consume(Painter):
    operator: Union[Operator, None] = None
    operands: Union[Tuple[Value], None] = None
    #canvas: Union[SeqCanvas, None] = None  # what to paint on
    #addr: Hashable = None                  # where to paint result
    source: Union[CellRef, None] = None
    dest: Union[CellRef, None] = None   # where to paint result

    # NEXT add source and dest args; call from .go and .act
    def paint(
        self,
        fm: FARGModel,
        source: CellRef,
        dest: CellRef,
        builder: Union[Agent, None]=None
    ):
        # TODO throw if any members/args are missing
        print('CPAIN', builder)
        if builder is None:
            builder = self
        s0: SeqState = source.contents
        try:
            taken_avails, remaining_avails = s0.take_avails(self.operands)
        except ValueNotAvail as exc:
            # TODO builder=self even if builder overridden by caller?
            fm.build(Blocked(taggee=self, reason=exc), builder=self)
            return
        result = self.operator.call(*taken_avails)
        new_avails = tuple(remaining_avails) + (result,)
        delta = ArithDelta(tuple(taken_avails), result, self.operator)
        s1 = SeqState(new_avails, delta)
        return fm.paint_value(dest, s1, builder=builder)

    '''
        # TODO Throw exception if any members (args) are missing
        s0: SeqState = self.canvas[self.addr - 1]
        taken_avails, remaining_avails = s0.take_avails(self.operands)
        result = self.operator.call(*taken_avails)
        new_avails = tuple(remaining_avails) + (result,)
        delta = ArithDelta(tuple(taken_avails), result, self.operator)
        s1 = SeqState(new_avails, delta)
        fm.paint_value(self.canvas, self.addr, s1)
    '''

    def OLDgo(self, fm: FARGModel):
        print('CONSGO', self.dest)
        #s0: SeqState = self.canvas[self.addr - 1]
        if self.dest is None:
            raise NotImplementedError('Need to decide what to do when a Consume is asked to .go without a .dest specified.')
        s0: SeqState = self.source_state()
        try:
            taken_avails, remaining_avails = s0.take_avails(self.operands)
        except ValueNotAvail as exc:
            fm.build(Blocked(taggee=self, reason=exc), builder=self)
            return
        result = self.operator.call(*taken_avails)
        new_avails = tuple(remaining_avails) + (result,)
        delta = ArithDelta(tuple(taken_avails), result, self.operator)
        s1 = SeqState(new_avails, delta)
        #fm.build(LiteralPainter(self.canvas, self.addr, s1), builder=self)
        fm.build(LiteralPainter(self.dest, s1), builder=self)

    def go(
        self,
        fm: FARGModel,
        source: Union[CellRef, None]=None,
        dest: Union[CellRef, None]=None,
        builder: Union[Agent, None]=None
    ):
        if source is None:
            source = self.source
        if dest is None:
            dest = source.imaginary_next_cellref()
        return self.paint(fm, source=source, dest=dest, builder=builder)
        
    def source_state(self):
        if self.source is None:
            return self.dest.preceding_contents()
        else:
            return self.source.contents

    def __str__(self):
        cl = self.__class__.__name__
        os = ' '.join(str(o) for o in [self.operator] + as_list(self.operands))
        # TODO Include canvas and addr
        xs = [os]
        if self.source is not None:
            xs.append(f'source={self.source}')
        if self.dest is not None:
            xs.append(f'dest={self.dest}')
        return f"{cl}({', '.join(xs)})"

@dataclass(frozen=True)
class Detector:
    target: Value  # Change to a match function?
    action: Callable

    def go(self, fm: FARGModel):
        # TODO See if self.target is there, favoring new elems

        found = fm.ws_query1(pred=HasAvailValue(self.target))
        print('FOUND', found)
        '''
        if True: # TODO Actually search
            de
            self.action(fm, detected)
        '''
        pass

    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.target}, {name_of(self.action)})'
        

def halt(fm: FARGModel):
    raise Halt

@dataclass(frozen=True)
class Want:
    target: Value
    canvas: SeqCanvas
    addr: Addr

    def go(self, fm: FARGModel):
        # TODO Don't build these if they're already built
        fm.build(Detector(self.target, action=halt))
        # TODO Get the Consume objects from a slipnet search
        #co = Consume(operator=plus, dest=CellRef(self.canvas, self.addr + 1))
        #co = Consume(operator=plus, source=CellRef(self.canvas, self.addr))
        co = Consume(
            operator=plus,
            source=CellRef(self.canvas, self.addr),
            dest=CellRef(self.canvas, self.addr).next()
        )
        for operands in ((4, 5), (4, 6), (9, 6)):
            fm.build(replace(co, operands=operands), builder=self)
        #fm.build(Consume(plus, (4, 5), self.canvas, self.addr + 1), builder=self)
        #fm.build(Consume(plus, (4, 6), self.canvas, self.addr + 1), builder=self)
        #fm.build(Consume(plus, (9, 6), self.canvas, self.addr + 1), builder=self)

    def __str__(self):
        cl = self.__class__.__name__
        # TODO Include canvas and addr
        return f'{cl}({self.target})'

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
        fm.pr(LiteralPainter)
        print(f'aseq0: {aseq0}')
        aseq0.go(fm)  #This should not complain

        '''
        aseq = fm.ws_query1(AgentSeq)
        aseq.act(fm)

        d15 = fm.ws_query1(Detector, target=15)
        d15.go(fm)

        '''
        print(fm)
