# Funcs.py -- Functions: objects to fill the third slot of a painter

from dataclasses import dataclass, field, fields, replace, InitVar, Field

from Types import Value, is_painter, painter_str, Fizzle
from Subst import Subst
from util import short

### The basic relational functions

def same(subst: Subst, v: Value) -> Value:
    return v

def succ(subst: Subst, v: Value) -> Value:
    # TODO Deal with 'z'
    if isinstance(v, str):
        return chr(ord(v) + 1)
    elif isinstance(v, int):
        return v + 1
    raise Fizzle("succ: Can't take successor of {v}")

def pred(subst: Subst, v: Value) -> Value:
    # TODO Deal with 'a'
    if isinstance(v, str):
        return chr(ord(v) - 1)
    elif isinstance(v, int):
        return v - 1
    raise Fizzle("pred: Can't take predecessor of {v}")

### The constant function

@dataclass(frozen=True)
class const:
    v: Value

    def __call__(self, subst: Subst, ignored: Value) -> Value:
        return self.v

    def short(self) -> str:
        cl = self.__class__.__name__
        if is_painter(self.v):
            return f'{cl}({painter_str(self.v)})'
        else:
            return f'{cl}({short(self.v)})'

