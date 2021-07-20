# FARGModel.py -- The class that holds a FARG model, and ancillary classes
# that are not specific to any one FARG model.

from pprint import pprint as pp
import inspect
from time import process_time
import csv

from dataclasses import dataclass, field, fields, replace, is_dataclass, InitVar
import dataclasses
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
from collections import Counter, defaultdict
from io import StringIO
from inspect import isclass
import inspect

import networkx as nx
import matplotlib.pyplot as plt
#import netgraph

from FMTypes import Elem, Value, Addr
from Slipnet import Slipnet, empty_slipnet
from FMGraphs import ActivationGraph
from util import is_iter, as_iter, as_list, pts, pl, pr, csep, ssep, \
    as_hashable, backslash, singleton, first, tupdict, as_dict, short, \
    sample_without_replacement, clip, reseed, default_field_value, d_subset, \
    fields_for, filter_none


def as_fmpred(o: Union[Type, Tuple, Callable, None]) -> Callable:
    '''Returns a predicate function that takes two arguments: a FARGModel and
    an object.'''
    # TODO Document the many ways this thing constructs a function.
    if isclass(o):
        return lambda fm, x: isinstance(x, o)
    elif isinstance(o, tuple):
        preds = tuple(as_fmpred(p) for p in o)
        return lambda fm, x: any(p(fm, x) for p in preds)
    elif callable(o):
        if first_arg_is_fargmodel(o):
            return o
        else:
            return lambda fm, x: o(x)
    elif o is None:
        return lambda fm, x: True
    else:
        return lambda fm, x: match_wo_none(x, o)
#        raise ValueError(
#            f'as_pred: {repr(o)} must be a class, a callable, or None.'
#        )

def first_arg_is_fargmodel(o: Callable) -> bool:
    p0 = first(inspect.signature(o).parameters.values())
    #print('FIRARG', o, p0, p0.annotation, type(p0.annotation))
    try:
        # TODO What if annotation is a string?
        return issubclass(p0.annotation, FARGModel)
    except TypeError:
        return False

def match_wo_none(other, obj_template) -> bool:
    '''Does obj_template == other if we ignore any fields in obj_template
    with a value of None? If other is an object of a subclass of obj_template's
    class, that also counts as a match.'''
    if not isinstance(other, obj_template.__class__):
        return False
    if not is_dataclass(obj_template) or not is_dataclass(other):
        return obj_template == other
    other_d = dataclasses.asdict(other)
    return all(
        v is None or v == other_d.get(k, None)
            for k, v in dataclasses.asdict(obj_template).items()
    )

# Element classes

@dataclass(frozen=True)
class Agent(ABC): #(Elem):
    '''A workspace element that does things.'''

    #orientation: Dict[str, Hashable]

    @abstractmethod
    # TODO Maybe put desired args explicitly in go()'s signature
    def go(cls, fm: 'FARGModel', contents, orientation):
        pass

    @abstractmethod
    # TODO Default impl should check if Agent is Blocked
    def can_go(cls, fm: 'FARGModel', contents, orientation) -> bool:
        pass

@dataclass
class ElemInWS:
    '''An element in the workspace.'''
    elem: Elem
    builder: Union[Agent, None]
        # .builder should not be used by Agents; it's strictly for debugging
        # and reporting on model behavor.
    behalf_of: List[Agent] = field(init=False, default_factory=list)
        # .behalf_of is all the Agents that want this Elem to exist, including
        # but not limited to its builder.
    tob: int   # time of birth (when Elem was added to the ws)
    # activation: float = 1.0
    # overrides for contents?
    # overrides for orientation?

    def add_behalf_of(self, agents):
        self.behalf_of += as_iter(agents)

    def __str__(self):
        return f'{self.elem}  builder={self.builder} tob={self.tob}'

# Exceptions

@dataclass(frozen=True)
class ValuesNotAvail(Exception):
    container: Hashable
    avails: Tuple[Hashable]
    unavails: Tuple[Hashable]

# Generic FARG model

@dataclass
class FARGModel:
    ws: Dict[Elem, ElemInWS] = field(default_factory=dict)
    t: int = 0
    slipnet: Slipnet = empty_slipnet  # TODO copy empty_slipnet?
    seed: Union[int, None] = None

    activation_g: ActivationGraph = field(
        default_factory=ActivationGraph, init=False
    )
    sleeping: InitVar[Dict[Elem, int]] = None # = field(
#        default_factory=dict, init=False
#    )

    mutual_support_weight: float = 1.0
    mutual_antipathy_weight: float = -0.2

    globals: Dict[str, Any] = field(default_factory=dict, init=False)

    # Whole-codelet functions

    def build(self, *args, **kwargs) -> Elem:
        '''The arguments specify an Elem to build in the workspace. If such
        an Elem already exists, we don't build anything. Returns the built
        or found Elem.'''
        if not args:
            raise NotImplementedError('still need to provide object to .build')
        if isclass(args[0]):
            raise NotImplementedError(".build can't yet construct the object for you")
        # TODO with logging('BUILT', args, kwargs)
        obj = args[0]
        builder = kwargs.pop('builder', None)
        init_a = kwargs.pop('init_a', None)
        if obj is None:  # attempting to build None builds nothing
            return None
        eiws = self.get_eiws(obj)
        if eiws is None:  # the Elem is not there, so now we really build it
            self.ws[obj] = eiws = ElemInWS(obj, builder=builder, tob=self.t)
            if init_a is None:
                if builder is None:
                    init_a = 1.0
                else:
                    init_a = min(1.0, self.a(builder))
            self.activation_g.add_node(obj, a=init_a)
            # create antipathy between obj and its enemies
            for elem in self.elems(HasAntipathyTo(obj, ignore=builder)):
                self.add_mutual_antipathy(obj, elem)
            '''
            # .on_build
            try:
                obj.on_build(self)
            except AttributeError:
                pass
            '''
        else:  # the Elem is already there, so don't build a new one
            obj = eiws.elem
        if builder:
            eiws.add_behalf_of(builder)
            self.add_mutual_support(builder, obj)
        return obj

    def paint(self, cr: 'CellRef', v: Value):
        cr.paint(v)

    def run(self, agent: Agent, **kwargs):
        # TODO Not if agent can't go?
        # TODO Not if agent doesn't exist?
        agent.go(self, **kwargs)  # TODO Supply overrides from eiws?

    # Activation

    def a(self, node: Hashable) -> float:
        '''Current activation level of node.'''
        return self.activation_g.a(node)

    def ae_weight(self, from_node: Hashable, to_node: Hashable) -> float:
        '''Activation edge weight. 0.0 if either node does not exist.'''
        try:
            return self.activation_g.edges[from_node, to_node]['weight']
        except KeyError:
            return 0.0

    def sum_a(self) -> float:
        return sum(self.a(elem) for elem in self.elems())

    def boost(self, node: Hashable):
        self.activation_g.boost(node)

    def downboost(self, node: Hashable):
        # HACK to make a node quiet down for a bit after it's done something;
        # better to explicitly sleep for a few timesteps?
        self.activation_g.nodes[node]['a'] /= 2.0

    def deactivate(self, node: Hashable):
        self.activation_g.nodes[node]['a'] = 0.0

    # Support

    def add_mutual_support(
        self, a: Hashable, b: Hashable, weight: Union[float, None]=None
    ):
        if weight is None:
            weight = self.mutual_support_weight
        self.activation_g.add_edge(a, b, weight=weight)
        self.activation_g.add_edge(b, a, weight=weight)

    set_mutual_support = add_mutual_support

    def set_support_edge(
        self, a: Hashable, b: Hashable, weight: Union[float, None]=None
    ):
        if weight is None:
            weight = self.mutual_support_weight
        self.activation_g.add_edge(a, b, weight=weight)

    def add_mutual_antipathy(
        self, a: Hashable, b: Hashable, weight: Union[float, None]=None
    ):
        if weight is None:
            weight = self.mutual_antipathy_weight
        self.activation_g.add_edge(a, b, weight=weight)
        self.activation_g.add_edge(b, a, weight=weight)

    # Querying

    def is_tagged(self, elems, tagpred) -> bool:
        '''Are any of elems tagged with a tag matching tagpred?'''
        # TODO Optimize; this now searches the entire ws
        return any(
            self.is_tagging(tag, elem)
                for elem in as_iter(elems)
                    for tag in self.elems(pred=tagpred)
        )

    def is_tagging(self, tag: Elem, elem: Elem) -> bool:
        try:
            return tag.is_tagging(elem)
        except AttributeError:
            return elem in self.taggees_of(tag)

    def taggees_of(self, tag: Elem) -> Iterable[Elem]:
        try:
            yield tag.taggees_of()
        except AttributeError:
            yield tag.taggee
        except AttributeError:
            yield from tag.taggees
        except AttributeError:
            return

    def is_blocked(self, elems) -> bool:
        return self.is_tagged(elems, Blocked)

    # Ancillary functions, callable by codelets and Agents

    def elems(self, pred=None, es=None) -> Iterable[Elem]:
        '''Returns a generator for *all* matches of pred. Unlike .ws_query(),
        .elems() does not make a weighted choice.'''
        fmpred = as_fmpred(pred)
        if es is None:
            es = self.ws.keys()
        return (e for e in as_iter(es) if fmpred(self, e))

    def get_eiws(self, obj) -> Union[ElemInWS, None]:
        '''Returns the ElemInWS from the workspace if it exists, otherwise
        None.'''
        return self.ws.get(obj, None)

    def builder_of(self, elem: Elem) -> Union[Elem, None]:
        try:
            eiws = self.ws[elem]
        except KeyError:
            return None
        return eiws.builder

    def has_antipathy_to(self, a: Hashable, b: Hashable) -> bool:
        try:
            return a.has_antipathy_to(b)
        except AttributeError:
            return False

    # Debugging and reporting

    def the(self, pred, es=None) -> Union[Elem, None]:
        '''Returns the first element from .elems(), or None if there isn't
        one.'''
        return first(self.elems(pred=pred, es=es))

@dataclass(frozen=True)
class HasAntipathyTo:
    elem: Elem
    ignore: Union[Elem, None]=None

    def __call__(self, fm: FARGModel, elem) -> bool:
        if elem == self.ignore:
            return False
        else:
            return fm.has_antipathy_to(self.elem, elem)

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

    # TODO Put this into a WithAvails mix-in.
    def take_avails(self, values: Iterable[Value]) \
    -> Tuple[Iterable[Value], Iterable[Value]]:
        '''Returns (taken_avails, remaining_avails). Might raise
        ValuesNotAvail.'''
        remaining_avails = [] if self.avails is None else list(self.avails)
        taken_avails = []
        missing_avails = []
        for v in values:
            try:
                remaining_avails.remove(v)
            except ValueError:
                taken_avails.append(None)
                missing_avails.append(v)
            else:
                taken_avails.append(v)
                missing_avails.append(None)
        if any(t is None for t in taken_avails):
            raise ValuesNotAvail(
                self,
                tuple(taken_avails),
                tuple(missing_avails)
            )
        return (taken_avails, remaining_avails)

@dataclass(eq=False)
class Canvas(ABC):
    # TODO get an iterator of all CellRefs, search for a value

    @abstractmethod
    def __getitem__(self, addr: Addr) -> Value:
        pass

    @abstractmethod
    def __setitem__(self, addr: Addr, v: Value):
        pass

@dataclass(eq=False)
class SeqCanvas(Canvas):
    states: List[SeqState] = field(default_factory=list)

    def __getitem__(self, addr: Addr) -> Value:
        # TODO Handle addr that can't be found or is not an index
        #print('SEQCGET', addr, len(self.states))
        if addr < len(self.states):
            return self.states[addr]
        else:
            return None

    def __setitem__(self, addr: Addr, v: Value):
        # TODO Handle addr that doesn't work as a list index
        # TODO Accept a builder argument?
        while len(self.states) <= addr:
            self.states.append(None)
        self.states[addr] = v

@dataclass(frozen=True)
class CellRef:
    canvas: Union[Canvas, None] = None
    addr: Union[Addr, None] = None

    def paint(self, v: Value):
        self.canvas[self.addr] = v

    def take_avails(self, values: Iterable[Value]) \
    -> Tuple[Iterable[Value], Iterable[Value]]:
        '''Returns (taken_avails, remaining_avails). Might raise
        ValuesNotAvail.'''
        # TODO Require a determinate Collection, not just an Iterable (since
        # we might fail and need to put 'values' into an exception).
        cell = self.canvas[self.addr]
        if cell is None:
            raise ValuesNotAvail(self, values)
        return cell.take_avails(values)

@dataclass(frozen=True)
class LitPainter(Agent):
    cellref: CellRef
    value: Value

    def go(self, fm, contents, orientation):
        fm.paint(self.cellref, self.value)

    def can_go(self, fm, contents, orientation):
        # TODO Check that we have enough activation?
        return True

    def has_antipathy_to(self, other) -> bool:
        return (
            self is not other
            and
            isinstance(other, LitPainter)
            and
            self.cellref == other.cellref
        )

@dataclass(frozen=True)
class Blocked(Agent):
    taggee: Elem
    reason: Hashable

    def go(self, fm: FARGModel):
        # TODO The .reason might not have a .try_to_fix method (and probably
        # shouldn't).
        #self.reason.try_to_fix(fm, behalf_of=self.taggee, builder=self)
        #fm.build(GoIsDone(taggee=self))
        raise NotImplementedError

    def can_go(self, fm):
        return True

    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.taggee}, {self.reason})'

@dataclass(frozen=True)
class Operator:
    '''Computes the result when Consume consumes operands.'''
    func: Callable
    name: str

    def call(self, *operands) -> int:
        return self.func(*operands)

    def __str__(self):
        return self.name

@dataclass(frozen=True)
class Consume(Agent):
    operator: Union[Operator, None] = None
    operands: Union[Tuple[Value], None] = None
    source: Union[CellRef, None] = None  # where to get operands
    dest: Union[CellRef, None] = None    # where to paint result

    def go(self, fm, *args, **kwargs):
        try:
            taken_avails, remaining_avails = \
                self.source.take_avails(self.operands)
        except ValuesNotAvail as exc:
            # tag self as Blocked
            fm.build(Blocked(taggee=self, reason=exc), builder=self)
            return
        result = self.operator.call(*taken_avails)
        new_avails = tuple(remaining_avails) + (result,)
        delta = StateDelta(tuple(taken_avails), result, self.operator)
        s1 = SeqState(new_avails, delta)
        fm.build(LitPainter(self.dest, s1), builder=self)
        #TODO Mark that we're done

    def can_go(self, *args, **kwargs):
        return True  # TODO False if any blank args

"""
class Copycat(Agent):
    
    def go(self, fm, @@):
        '''
        What's missing? (Do we need to build the copy with blanks?)
        
        We are done when-- A copy is built? No, we might build competing
        copies. When our master is satisfied? No, we might have multiple
        masters.
        '''
"""
