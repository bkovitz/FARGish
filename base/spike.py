# spike.py -- A spike to guide development of Base.py

from __future__ import annotations
#from Base import FARGModel, Canvas, Avails
#from Base import Plus
from util import ps, pr

from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
import operator

from util import as_tuple, short

Value = Hashable

Tag = Value  # TODO

@dataclass
class NodeDataclassMixin:
    datum: Value
    args: Optional[ArgsMap] = None
    tags: Optional[Set[Tag]] = None

class Node(NodeDataclassMixin):

    def with_args_merged(self, args: ArgsMap) -> Node:
        '''Returns new Node, with 'args' overriding self.args.'''
        return Node(self.datum, ArgsMap.merged(self.args, args), self.tags)

    def short(self) -> str:
        result = short(self.datum)
        if self.args:
            result += ' ' + short(self.args)
        if self.tags:
            result += ' ' + short(self.tags)
        return result

@dataclass(frozen=True)
class ArgsMap:
    '''A dictionary of argument names and their values, suitable for passing
    to a function.'''
    d: ArgsD

    def __add__(self, other) -> Union[ArgsMap, Node]:
        if isinstance(other, ArgsMap):
            return ArgsMap(other.d | self.d)
        elif isinstance(other, Node):
            return other.with_args_merged(self)
        else:
            return Node(other, self, None)

    def short(self) -> str:
        return ' '.join(f'{short(k)}={short(v)}' for k, v in self.d.items())

    @classmethod
    def merged(cls, new: Args, old: Args) -> Union[ArgsMap, None]:
        '''Returns an ArgsMap (or None) with the values from 'new' overriding
        the values from 'old'.'''
        if new is None:
            if old is None:
                return None
            elif isinstance(old, ArgsMap):
                return old
            else:
                return ArgsMap(old)
        elif old is None:
            if isinstance(new, ArgsMap):
                return new
            else:
                return ArgsMap(new)
        else:
            dnew = new.d if isinstance(new, ArgsMap) else new
            dold = old.d if isinstance(old, ArgsMap) else old
            return ArgsMap(dold | dnew)

ArgsD = Dict[str, Value]
Args = Union[None, ArgsD, ArgsMap]

def Avails(*vs: Value) -> ArgsMap:
    return ArgsMap(dict(avails=as_tuple(vs)))


@dataclass(frozen=True)
class Operator:
    func: Callable
    name: str
    min_num_operands: int = 2

    def __hash__(self):
        '''It's necessary to omit self.func from __hash__ in order to maintain
        determinism. The default hash code of a function is its address in
        memory, which is non-deterministic. If necessary, one could include
        str(self.func).'''
        return hash(str(self))

    def mk_short(self, operands: Sequence[Value]) -> str:
        if operands:
            nm = f' {self.name} '
            return nm.join(short(o) for o in operands)
        else:
            return f'_{self.name}_'

@dataclass(frozen=True)
class Consume:  #(Codelet):
    name: str
    operator: Operator
    operands: Tuple[Value, ...]

    def short(self) -> str:
        return self.operator.mk_short(self.operands)


plus = Operator(operator.add, '+')
mult = Operator(operator.mul, '*')

def Plus(*operands: int) -> Consume:
    return Consume(name='Plus', operator=plus, operands=as_tuple(operands))


m = Avails(4, 5) + Plus(4, 5)
ps(m)
#fm = FARGModel()
#ca = fm.build(Canvas.make(
#    Avails(4, 5) + Plus(4, 5)
#))
#ps(ca)
##fm.run(ca)
##ps(ca)
