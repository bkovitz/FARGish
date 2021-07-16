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


@dataclass(frozen=True)
class Agent: #(Elem):
    '''A workspace element that does things.'''

    orientation: Dict[str, Hashable]

    @abstractmethod
    def go(cls, fm: 'FARGModel', contents, orientation):
        pass

    @abstractmethod
    # TODO Default impl should check if Agent is Blocked
    def can_go(cls, fm: 'FARGModel', contents, orientation):
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

    def __str__(self):
        return f'{self.elem}  builder={self.builder} tob={self.tob}'

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
            obj = eiws.elem
            # create antipathy between obj and its enemies
            ''' TODO
            for elem in self.elems(HasAntipathyTo(obj, ignore=builder)):
                self.add_mut_antipathy(obj, elem)
            # .on_build
            try:
                obj.on_build(self)
            except AttributeError:
                pass
            '''
        if builder:
            self.add_mut_support(builder, obj)
        return obj

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

    def add_mut_support(
        self, a: Hashable, b: Hashable, weight: Union[float, None]=None
    ):
        if weight is None:
            weight = self.mutual_support_weight
        self.activation_g.add_edge(a, b, weight=weight)
        self.activation_g.add_edge(b, a, weight=weight)

    set_mut_support = add_mut_support

    def set_support_edge(
        self, a: Hashable, b: Hashable, weight: Union[float, None]=None
    ):
        if weight is None:
            weight = self.mutual_support_weight
        self.activation_g.add_edge(a, b, weight=weight)

    def add_mut_antipathy(
        self, a: Hashable, b: Hashable, weight: Union[float, None]=None
    ):
        if weight is None:
            weight = self.mutual_antipathy_weight
        self.activation_g.add_edge(a, b, weight=weight)
        self.activation_g.add_edge(b, a, weight=weight)

    # Ancillary functions, callable by codelets and Agents

    def elems(self, pred=None, es=None) -> Iterable[Elem]:
        '''Returns a generator for *all* matches of pred. Unlike .ws_query(),
        .elems() does not make a weighted choice.'''
        raise NotImplementedError

    def get_eiws(self, obj) -> Union[ElemInWS, None]:
        '''Returns the ElemInWS from the workspace if it exists, otherwise
        None.'''
        return self.ws.get(obj, None)

    # Debugging and reporting

    def the(self, pred) -> Elem:
        '''Returns the only match of pred that really matters.'''
        # TODO
        pass

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

@dataclass(eq=False)
class Canvas(ABC):
    pass
    # TODO abstractmethods to paint and get values, and get an iterator of
    # all CellRefs.

@dataclass(eq=False)
class SeqCanvas(Canvas):
    states: List[SeqState] = field(default_factory=list)

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
