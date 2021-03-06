# Numbo.py -- Run this to run Numbo; main classes specific to Numbo

from dataclasses import dataclass, field, replace, asdict
import dataclasses
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence
import operator
from operator import itemgetter, attrgetter
from collections import Counter
import math
from numbers import Number

import matplotlib.pyplot as plt
import numpy as np

from FARGish2 import FARGModel, Elem, Value, SeqCanvas, Addr, Agent, AgentSeq, \
    Tag, TaggeeTag, \
    RaiseException, Blocked, BaseDetector, Detector, CellRef, ImCell, \
    SeqState, Halt, Glom, \
    StateDelta, ValueNotAvail, ValuesNotAvail, CellWithAvailValue, is_real, \
    GoIsDone, ActIsDone, match_wo_none, has_avail_value, dig_attr
from Slipnet import Slipnet, FeatureWrapper, IntFeatures
from util import is_iter, as_iter, as_list, pts, pl, pr, csep, ssep, \
    as_hashable, backslash, singleton, first, tupdict, as_dict, short, \
    sample_without_replacement


@dataclass(frozen=True)
class Operator:
    '''Computes the result when Consume consumes operands.'''
    func: Callable
    name: str

    def call(self, *operands) -> int:
        return self.func(*operands)

    def __str__(self):
        return self.name

plus = Operator(operator.add, '+')
times = Operator(operator.mul, 'x')
minus = Operator(operator.sub, '-')

@dataclass(frozen=True)
class Consume(Agent):
    operator: Union[Operator, None] = None
    operands: Union[Tuple[Value], None] = None
    source: Union[CellRef, None] = None  # where to get operands
    dest: Union[CellRef, None] = None    # where to paint result

    def on_build(self, fm: FARGModel):
        fm.build(self.source, builder=self)
        fm.build(self.dest, builder=self, edge_weight=1.0)

    def rebuild_with_blanks(
        self,
        fm: FARGModel,
        avail_operands: Tuple[Value],
        unavail_operands: Tuple[Value]
    ):
        print('REBU', self, avail_operands, unavail_operands)
        fm.build(replace(self, operands=avail_operands), builder=self)

    def paint(
        self,
        fm: FARGModel,
        source: CellRef,
        dest: CellRef,
        builder: Union[Agent, None]=None,
        **ignored
    ):
        #print('CPAINT', self)
        # TODO throw if any members/args are missing
        if builder is None:
            builder = self
        s0: SeqState = source.contents
        if s0 is None:
            return
        try:
            taken_avails, remaining_avails = s0.take_avails(self.operands)
        except ValuesNotAvail as exc:
            # TODO builder=self even if builder overridden by caller?
            #print('CNOT')
            fm.build(Blocked(taggee=self, reason=exc), builder=self)
            fm.downboost(self)
            return
        result = self.operator.call(*taken_avails)
        new_avails = tuple(remaining_avails) + (result,)
        delta = ArithDelta(tuple(taken_avails), result, self.operator)
        s1 = SeqState(new_avails, delta)
        return fm.paint_value(dest, s1, builder=builder)

    def can_go(self, fm):
        # TODO Return False if we painted an ImCell from .source
        return not fm.is_blocked(self)

    def go(
        self,
        fm: FARGModel,
        **overrides
        #source: Union[CellRef, None]=None,
        #dest: Union[CellRef, None]=None,
        #builder: Union[Agent, None]=None
    ) -> CellRef:
        if overrides.get('source', None) is None:
            overrides['source'] = self.source
        if overrides.get('dest', None) is None:
            overrides['dest'] = overrides['source'].imaginary_next_cellref()
        result = self.paint(fm, **overrides)
        if result is not None:
            #fm.boost(self)
            fm.build(GoIsDone(taggee=self))
        return result

    def can_act(self, fm):
        return (
            self.have_all_args()
            and
            is_real(self.source)
            and
            not fm.is_blocked(self)
        )

    def act(self, fm, **kwargs) -> CellRef:
        # TODO throw if there are any imaginary CellRefs
        if kwargs.get('source', None) is None:
            kwargs['source'] = self.source
        if kwargs.get('dest', None) is None:
            kwargs['dest'] = kwargs['source'].next()
        result = self.paint(fm, **kwargs)
        fm.build(ActIsDone(taggee=self))
        return result
        
    def have_all_args(self) -> bool:
        return (
            self.operator is not None
            and
            self.operands
        )

    # TODO rm
    def source_state(self):
        if self.source is None:
            return self.dest.preceding_contents()
        else:
            return self.source.contents

    # TODO UT
    def has_antipathy_to(self, other: Union[Elem, None]) -> bool:
        return (
            self is not other
            and
            isinstance(other, Consume)
            and
            self.source == other.source
        )

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

    def features(self) -> Iterable[Hashable]:
        for operand in self.operands:
            yield operand
            yield Before(operand)
        yield self.operator
        result = self.operator.call(*self.operands)
        yield result
        yield After(result)
        if all(result > operand for operand in self.operands):
            yield Increase()
        elif any(result < operand for operand in self.operands):
            yield Decrease()
        counter = Counter(self.operands)
        for operand, count in counter.items():
            if count == 2:
                yield Doubled(operand)
                yield Doubled()
        yield NumOperands(len(self.operands))
        mino = min(self.operands)
        maxo = max(self.operands)
        yield MinBefore(mino)
        yield MaxBefore(maxo)
        if mino == maxo:
            yield OneUniqueBefore(mino)
        elif set(range(mino, maxo + 1)) == set(self.operands):
            yield SequentialBefore(mino, maxo)

@dataclass(frozen=True)
class TakeAvailsScout(Agent):
    
    def go(self, fm, **overrides):
        # TODO Make a Consume (or whatever) with existing avails
        pass

    def can_go(self, fm):
        # TODO Self-destruct when there are no more avail combinations from
        # which to make new and different Consumes?
        return True

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
class SolvedNumble(Halt):
    cellref: 'CellRef'

    def __str__(self):
        return self.cellref.canvas.as_solution()

# TODO UT
@dataclass(frozen=True)
class Exclude:
    '''A filter predicate: returns True iff its argument is not in .items,
    using a weaker condition than strict equality. This enables excluding
    Consume objects only on the basis of their non-null fields.'''
    items: Collection[Hashable]

    def __call__(self, x: Hashable) -> bool:
        if isinstance(x, Consume):  #HACK
            return not any(
                (isinstance(item, Consume)
                 and
                 item.operands == x.operands
                 and
                 item.operator == x.operator
                ) for item in self.items
            )
        else:
            return x not in self.items

@dataclass(frozen=True)
class GettingCloser(TaggeeTag):
    target: Number = None
    weight: float = None

    @dataclass(frozen=True)
    class Tagger(BaseDetector):
        target: Number

        def look(self, fm: FARGModel):
            # TODO Exclude nodes already tagged GettingCloser
            found = first(fm.search_ws(ImCell))
            #print('GettingCloser.Tagger FOUND', found)
            if found:
                weight = GettingCloser.calc_weight(found, self.target)
                if weight > 0.001:
                    # TODO Indicate the Want node? The target?
                    # TODO Indicate builder
                    tag = fm.build(GettingCloser(
                        taggee=found, target=self.target, weight=weight
                    ))
                    fm.add_mut_support(tag, found)

    @classmethod
    def calc_weight(cls, node: Hashable, target: Number) \
    -> float:
        arith_delta = dig_attr(node, 'last_move')
        before = dig_attr(arith_delta, 'before')
        after = dig_attr(arith_delta, 'after')
        if before is None or after is None:
            return 0.0
        try:
            start_dist = min(abs(target - b) for b in before)
        except ValueError:
            return 0.0
        after_dist = abs(target - after)
        closer_by = start_dist - after_dist
        if closer_by <= 0.0:
            return 0.0
        return closer_by / start_dist

    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.taggee}, target={self.target}, weight={self.weight})'

@dataclass(frozen=True)
class Want(Agent):
    '''Represents the pressure to find or construct a value on a canvas.'''
    target: Value = None
    canvas: SeqCanvas = None
    addr: Addr = None  # Addr of the start state, before

    max_a: ClassVar[float] = 4.0

    def on_build(self, fm: FARGModel):
        fm.add_mut_support(self, self.canvas)

    def act(self, fm: FARGModel):
        pass

    def can_go(self, fm: FARGModel):
        return not fm.is_blocked(self)

    def go(self, fm: FARGModel):
        # TODO Don't build these if they're already built
        fm.build(
            Detector(self.target, action=RaiseException(SolvedNumble)),
            builder=self
        )
        fm.build(
            GettingCloser.Tagger(target=self.target),
            builder=self
        )
        self.consult_slipnet(fm)
        self.update_support(fm)
        fm.sleep(self)

    def consult_slipnet(self, fm: FARGModel):
        source = self.canvas.last_nonblank()
        #avails = self.canvas[self.addr].avails
        avails = source.contents.avails
        activations_in = {}
        for avail in avails:
            activations_in[Before(avail)] = 1.0
        activations_in[After(self.target)] = 0.1
        if all(avail > self.target for avail in avails):
            activations_in[Increase()] = 10.0

        exclude = Exclude(fm.neighbors(self))
        #source = CellRef(self.canvas, self.addr)
        agents = fm.pulse_slipnet(
            # GLOBAL constants in next line
            activations_in, k=20, type=Agent, num_get=1, filter=exclude
        )
        # TODO We need a way to get an Agent from the slipnet that means
        # something like "Just add what you have."
        #print('WANT got from slipnet:', [str(a) for a in agents]) #DIAG
        for agent in agents:
            if isinstance(agent, Consume):  #HACK
                agent = replace(agent, source=source)
            fm.build(agent, builder=self, init_a=0.1)

    def update_support(self, fm: FARGModel):
        for consume in fm.search_ws(
            (Consume, CellRef, GettingCloser(taggee=None, target=self.target)),
            max_n=20
        ):
            fm.set_support_edge(
                self, consume, weight=self.promisingness_of(fm, consume)
            )

    def promisingness_of(self, fm: FARGModel, elem: Elem) -> float:
        # TODO Scouts, probably constructed from the slipnet, should search
        # for what's promising and apply tags. It shouldn't be hard-coded
        # in the Want class.
        if isinstance(elem, Consume):
            result = 0.0
            if fm.is_blocked(elem):
                result += 0.1
            else:
                result += 0.2
                if fm.can_act(elem):
                    result += 0.2
            return result
        elif isinstance(elem, CellRef):
            #return 2.0 if elem.contents == self.target else 0.0
            return 20.0 if has_avail_value(elem.contents, self.target) else 0.0
        elif isinstance(elem, GettingCloser): # TODO Promising, not GettingCloser
            #print('WANTGC', elem)  #DIAG
            return 10 * elem.weight
        else:
            return 0.0
            
    def __str__(self):
        cl = self.__class__.__name__
        # TODO Include canvas and addr
        return f'{cl}({self.target})'

@dataclass(frozen=True)
class Increase:
    pass

@dataclass(frozen=True)
class Decrease:
    pass

class Before(FeatureWrapper):
    pass

class After(FeatureWrapper):
    pass

class MinBefore(FeatureWrapper):
    pass

class MaxBefore(FeatureWrapper):
    pass

class Doubled(FeatureWrapper):
    pass

class NumOperands(FeatureWrapper):
    pass

class OneUniqueBefore(FeatureWrapper):
    pass

@dataclass(frozen=True)
class SequentialBefore:
    lb: Any
    ub: Any

class SlipnetWithInt(IntFeatures, Slipnet):
    pass

class Numbo(FARGModel):

    def make_slipnet(self):
        self.slipnet = SlipnetWithInt(
            Consume(operator, (a, b))
                for a in range(1, 11)
                for b in range(1, 11)
                for operator in [plus, times, minus]
                    if a >= b
        )

    # TODO rm this; should only happen in base class
#    def nodes_to_log(self):
#        return self.elems(Consume(operands=(6, 4)))

@dataclass
class NumberLine:
    lb: float
    ub: float
    peaks: List[float]
    peakwidth: float

    #TODO noise
    #TODO scaling   log or linear
    def f(self, x: float) -> float:
        return sum(self.peakf(peak, x) for peak in self.peaks)

    def peakf(self, peak, x) -> float:
        return (
            (self.peakwidth / 2.0)
            /
            (math.pi * (x - peak)**2 + (self.peakwidth / 2.0)**2)
        )

def r4_5_6__15(*args, **kwargs):
    global fm, ca, wa
    fm = Numbo(*args, **kwargs)
    ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
    wa = fm.build(Want(15, canvas=ca, addr=0))
    fm.do_timestep(num=19)
    pr(fm, edges=True)
    print()
    #fm.pr_flows()
    print(f'seed={fm.seed}')
    

if __name__ == '__main__':
    from FARGish2 import CanGo, CanAct

    if False:
        fm = Numbo()
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

    if False:
        fm = Numbo()
        c = Consume(operands=(5, 4), operator=plus)
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        avails = ca[0].avails
        keys = [Before(n) for n in avails]
        keys.append(After(15))
        #wa = fm.build(Want(15, canvas=ca, addr=0))

        '''
        co = Consume(
            operator=plus,
            source=CellRef(ca, 0),
            dest=CellRef(ca, 0).next()
        )

        for operands in ((4, 5), (4, 6), (9, 6)):  # HACK
            fm.build(replace(co, operands=operands), builder=None)
        print(fm.agents())
        '''

        a_in = {
            Before(4): 1.0,
            Before(5): 1.0,
            Before(6): 1.0,
            After(15): 0.1,
            Increase(): 10.0
        }
        #q = fm.slipnet.query(keys, type=Agent, k=20, filter=Exclude(fm.agents()))
        #q = fm.slipnet.query(activations_in=a_in, type=Agent, k=20, filter=Exclude(fm.agents()))
        q = fm.slipnet.top(fm.slipnet.dquery(activations_in=a_in), k=50)
        pts(q)
        '''
        agent = list(sample_without_replacement(
            [nas.node for nas in q],
            k=1,
            weights=[nas.a for nas in q]
        ))
        print(agent)
        '''

    if False:
        fm = Numbo()
        c = Consume(operands=(5, 4), operator=plus)
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        wa = fm.build(Want(15, canvas=ca, addr=0))
        #wa.go(fm)
        fm.do_timestep(num=20)
        pr(fm, edges=True)

        print()
        fm.pr(fm.search_ws(Consume, max_n=5))
        print()
        w = first(fm.elems(Want))
        cs = list(fm.elems(Consume))
        pts(cs)

    if False:
        fm = Numbo()
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        wa = fm.build(Want(15, canvas=ca, addr=0))
        cr0 = CellRef(ca, 0)
        cr1 = CellRef(ca, 1)
        c1 = fm.build(Consume(operands=(5, 4), operator=plus, source=cr0))
        c2 = fm.build(Consume(operands=(9, 6), operator=plus, source=cr1))
        fm.t = 9
        print('C1', fm.can_act(c1))
        fm.do_timestep(c1)
        fm.do_timestep(c2)
        fm.do_timestep(wa)
        fm.pr(edges=True)

    if False:
        pf = NumberLine(lb=1, ub=10, peaks=[2.0, 3.0], peakwidth=2.0)
        
        plt.ion()
        plt.xlabel('f')
        plt.ylabel('x')
        xs = np.linspace(pf.lb, pf.ub, 200)
        plt.plot(xs, [pf.f(x) for x in xs])
#        for node, series in d.items():
#            plt.plot(*zip(*series), label=node)
        #plt.axis([0, max_t, 0, max_a])
        #plt.legend()

    if False:
        fm = Numbo()
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        cr = CellRef(ca, 0)
        avails = cr.contents.avails

        pf = NumberLine(lb=1, ub=10, peaks=[4.0], peakwidth=2.0)
        g = Glom.make_from(pf.f, avails)

    if True:
        r4_5_6__15(seed=23686273699696067)
        ic = first(fm.elems(ImCell))
        print('CALC', GettingCloser.calc_weight(ic, 15))
