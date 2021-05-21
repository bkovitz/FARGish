# Numbo.py -- Run this to run Numbo; main classes specific to Numbo

from dataclasses import dataclass, field, replace
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence
import operator
from operator import itemgetter, attrgetter

from FARGish2 import FARGModel, Value, SeqCanvas, Addr, Agent, AgentSeq, \
    RaiseException, Blocked, Detector, CellRef, SeqState, Halt, \
    StateDelta, ValueNotAvail, CellWithAvailValue
from util import is_iter, as_iter, as_list, pts, pl, csep, ssep, as_hashable, \
    backslash, singleton, first, tupdict, as_dict, short


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

    def paint(
        self,
        fm: FARGModel,
        source: CellRef,
        dest: CellRef,
        builder: Union[Agent, None]=None,
        **ignored
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
        return self.paint(fm, **overrides)

    def act(self, fm, **kwargs) -> CellRef:
        # TODO throw if there are any imaginary CellRefs
        if kwargs.get('source', None) is None:
            kwargs['source'] = self.source
        if kwargs.get('dest', None) is None:
            kwargs['dest'] = kwargs['source'].next()
        return self.paint(fm, **kwargs)
        
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

@dataclass(frozen=True)
class Want(Agent):
    target: Value
    canvas: SeqCanvas
    addr: Addr

    def act(self, fm: FARGModel):
        pass

    def go(self, fm: FARGModel):
        # TODO Don't build these if they're already built
        fm.build(Detector(self.target, action=RaiseException(SolvedNumble)))
        # TODO Get the Consume objects from a slipnet search
        #co = Consume(operator=plus, dest=CellRef(self.canvas, self.addr + 1))
        #co = Consume(operator=plus, source=CellRef(self.canvas, self.addr))
        co = Consume(
            operator=plus,
            source=CellRef(self.canvas, self.addr),
            dest=CellRef(self.canvas, self.addr).next()
        )
        # NEXT Consult the slipnet
        for operands in ((4, 5), (4, 6), (9, 6)):  # HACK
            fm.build(replace(co, operands=operands), builder=self)
        #fm.build(Consume(plus, (4, 5), self.canvas, self.addr + 1), builder=self)
        #fm.build(Consume(plus, (4, 6), self.canvas, self.addr + 1), builder=self)
        #fm.build(Consume(plus, (9, 6), self.canvas, self.addr + 1), builder=self)

    def __str__(self):
        cl = self.__class__.__name__
        # TODO Include canvas and addr
        return f'{cl}({self.target})'

if __name__ == '__main__':
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
