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

from FMTypes import Elem, Value, Addr, FMPred
from Slipnet import Slipnet, empty_slipnet
from FMGraphs import ActivationGraph
from util import is_iter, as_iter, as_list, pts, pl, pr, csep, ssep, \
    as_hashable, backslash, singleton, first, tupdict, as_dict, short, \
    sample_without_replacement, clip, reseed, default_field_value, d_subset, \
    fields_for, filter_none


# Global functions

def as_fmpred(o: FMPred) -> Callable:
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

def has_avail_value(
    elem: Union['CellRef', Elem, None],
    v: Value
) -> Union[bool, None, 'CellRef', Elem]:
    if elem is None:
        return False
    try:
        #return elem.has_avail_value(v)
        return any(elem.has_avail_value(v1) for v1 in as_iter(v))
    except AttributeError:
        if elem == v:
            return elem
        else:
            return False

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

    def update_support(cls, fm: 'FARGModel'):
        '''The Agent should update the weights from itself to other Elems.
        Default implementation: do nothing.'''
        # TODO Return an iterable of updates so they're easy to log.
        pass

@dataclass(frozen=True)
class Detector(ABC):

    @abstractmethod
    def look(self, fm: 'FARGModel'):
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
        agent.update_support(self)

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

    def search_ws(
        self,
        pred: Union[Type, Callable, None]=None,
        min_a: Union[float, None]=None,
        max_n: int=1
    ) -> Iterable[Elem]:
        '''Returns generator of up to max_n nodes that match pred,
        chosen randomly, weighted by activation.'''
        elems = self.elems(pred)
        if min_a is not None:
            elems = (e for e in elems if self.a(e) >= min_a)
        elems = list(elems)
        activations = [self.a(e) for e in elems]
        yield from sample_without_replacement(
            elems, weights=activations, k=max_n
        )

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

    def degree(self, a: Elem) -> int:
        #return self.activation_g.degree(a)
        return len(self.neighbors(a))

    def neighbors(self, e: Elem) -> List[Elem]:
        g = self.activation_g
        return set(list(g.successors(e)) + list(g.predecessors(e)))


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

    def sleep(self, elem: Elem, num_timesteps=2):
        if elem in self.sleeping:
            self.sleeping[elem] += num_timesteps
        else:
            # TODO only if elem is in the ws
            self.sleeping[elem] = self.t + num_timesteps

    # Debugging and reporting

    def the(self, pred, es=None) -> Union[Elem, None]:
        '''Returns the first element from .elems(), or None if there isn't
        one.'''
        return first(self.elems(pred=pred, es=es))

    def __repr__(self):
        cl = self.__class__.__name__
        return f'<{cl} object, t={self.t}>'

    def __str__(self):
        result = StringIO()
        print(f't={self.t}  sum_a={self.sum_a():2.3f}', file=result)
        #print(self.elines(self.elems(), tofile=result))
        self.pr(tofile=result, show_n=True)
        return result.getvalue()

    def l1str(self, eiws: Union[ElemInWS, Elem], indent=None) -> str:
        '''The one-line string for a ws elem, showing its activation.'''
        if indent is None:
            indent = '  '
        if not isinstance(eiws, ElemInWS):
            eiws = self.ws[eiws]  # TODO if the elem does not exist
        return f'{indent}{self.a(eiws.elem): 7.3f}  {eiws} deg={self.degree(eiws.elem)}'

    def e1str(self, node1: Elem, node2: Elem, indent=None) -> str:
        '''The one-line string for the edge from node1 to node2. Does not
        show node1. Indented one level further than 'indent'.'''
        if indent is None:
            indent = '  '
        outgoing_weight = self.ae_weight(node1, node2)
        incoming_weight = self.ae_weight(node2, node1)
        if outgoing_weight != 0.0:
            if incoming_weight != 0.0:
                arrow = f'{outgoing_weight: 6.3f} <--> {incoming_weight: 6.3f}'
            else:
                arrow = f'{outgoing_weight: 6.3f}  -->       '
        else:
            arrow = f'       <--  {incoming_weight: 6.3f}'
        #return f'{indent}  {weight: 7.3f} --> {node2}  a={self.a(node2):2.3f}'
        return f'{indent}  {arrow} {node2}  a={self.a(node2):2.3f}'

    def pr(
        self,
        pred: Union[Type, Callable, None]=None,
        es=None,  # Elem, Elems, or None for all Elems
        tofile=None,
        indent=None,
        show_n=False,
        edges=False,
        **kwargs
    ):
        '''Prints a subset of the workspace.'''
        count = 0
        for s, elem in sorted(
            (self.l1str(elem, indent), elem)
                for elem in self.elems(pred=pred, es=es)
        ):
            count += 1
            print(s, file=tofile)
            if edges:
                for e in sorted(
                    self.e1str(elem, neighbor)
                        for neighbor in self.neighbors(elem)
                ):
                    print(' ', e, file=tofile)
        if show_n:
            print(f'n={count}', file=tofile)

    def pr_flows(self):
        print(f'FLOWS t={self.t}')
        self.activation_g.pr_flows()


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

    def has_avail_value(self, v):
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

    def __str__(self):
        return f"SeqCanvas({'; '.join(str(st) for st in self.states)})"

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

    def has_avail_value(self, v):
        return has_avail_value(self.canvas[self.addr], v)

    def __str__(self):
        cl = self.__class__.__name__
        return f'canvas[{self.addr}]'

@dataclass(frozen=True)
class RaiseException:
    exc_class: Type

    def __call__(self, fm: FARGModel, *args, **kwargs):
        raise self.exc_class(*args, **kwargs)

    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.exc_class.__name__})'

@dataclass(frozen=True)
class AvailDetector(Detector):
    target: Union[Value, Collection[Value]]
    filter: FMPred  # Detector looks only at elements that match this predicate
    action: Callable

    # TODO Don't look at things previously found
    def look(self, fm):
        # TODO Should has_avail_value be included in pred?
        found = set(fm.search_ws(self.filter))
        # TODO log found
        for elem in found:
            if has_avail_value(elem, self.target):
                # TODO action should be a codelet
                self.action(fm, elem)
    
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

    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.cellref}, {self.value})'

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
class Operator(ABC):
    '''Computes the result when Consume consumes operands.'''
    func: Callable
    name: str

    statedelta_ctor: ClassVar[Callable] = StateDelta

    def call(self, *operands) -> int:
        return self.func(*operands)

    def consume(self, source_state: 'SeqState', operands: Collection[Value]) \
    -> 'SeqState':
        '''Should try to consume operands from the avails in source_state,
        returning the resulting SeqState. For example, a 'plus' Operator
        consuming (4, 5) should return a SeqState with the 4 and 5 removed
        and replaced with a 9, and a StateDelta describing that. If any
        of the operands are not avail in source_state, then should raise
        ValuesNotAvail.'''
        taken_avails, remaining_avails = source_state.take_avails(operands)
        result = self.call(*taken_avails)
        new_avails = tuple(remaining_avails) + (result,)
        delta = self.statedelta_ctor(tuple(taken_avails), result, self)
        return SeqState(new_avails, delta)

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

        try:
            s1 = self.operator.consume(self.source, self.operands)
        except ValuesNotAvail as exc:
            # tag self as Blocked
            fm.build(Blocked(taggee=self, reason=exc), builder=self)
            return
        fm.build(LitPainter(self.dest, s1), builder=self)
        #TODO Mark that we're done

    def can_go(self, *args, **kwargs):
        return True  # TODO False if any blank args

    def __str__(self):
        cl = self.__class__.__name__
        os = ' '.join(str(o) for o in [self.operator] + as_list(self.operands))
        # TODO Include canvas and addr
        xs = [os]
        if self.source is not None and self.dest is not None:
            xs.append(f'{self.source}->{self.dest}')
        else:
            if self.source is not None:
                xs.append(f'source={self.source}')
            if self.dest is not None:
                xs.append(f'dest={self.dest}')
        return f"{cl}({', '.join(xs)})"

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
