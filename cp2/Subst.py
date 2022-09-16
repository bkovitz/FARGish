# Subst.py -- 'Subst' and related classes

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod
from functools import reduce
import operator

from pyrsistent import pmap
from pyrsistent.typing import PMap

from Types import Expr, Index, FizzleNotIndex, SoupRef, Variable, \
    as_index, is_painter
from Log import lo
from util import force_setattr, short


class HasAsIndex(ABC):

    @abstractmethod
    def as_index(self) -> Index:
        pass

@dataclass(frozen=True)
class Plus(HasAsIndex):
    args: Tuple[Expr]

    def __init__(self, *args: Expr):
        force_setattr(self, 'args', args)

    def __str__(self) -> str:
        return '+'.join(str(a) for a in self.args)

    def value_of(self, subst: Subst) -> Expr:
        return self.simplify(subst, self.args)

    @classmethod
    @no_type_check  # mypy 0.971 crashes on '*more' below
    def simplify(cls, subst: Subst, args: Tuple[Expr], init: int=0) -> Expr:
        '''Evaluates 'args' as a sum to as simple a form as possible. The
        simplest form is an int; if a variable in 'args' is undefined, then
        we return an Expr containing that variable.'''
        match args:
            case ():
                return init
            case (expr,):
                return cls.try_to_add(init, subst.simplify(expr))
            case (expr, *more):
                #lo('init=', init, 'expr=', expr, subst)
                return cls.simplify(
                    subst,
                    more,
                    cls.try_to_add(init, subst.simplify(expr))
                )

    @classmethod
    def try_to_add(cls, a: Any, b: Any) -> Expr:
        match (a, b):
            case (0, _):
                return b
            case (int(), int()):
                return a + b
            case (Plus(args_a), Plus(args_b)):
                return Plus(*(args_a + args_b))
            case (Plus(args_a), _):
                return Plus(*(args_a + (b,)))
            case (_, Plus(args_b)):
                return Plus(*((a,) + args_b))
            case _:
                return Plus(a, b)

    def as_index(self) -> Index:
        return reduce(operator.add, (as_index(arg) for arg in self.args), 0)

@dataclass(frozen=True)
class Subst:
    '''A substitution table, i.e. a mapping from Variables to Exprs.'''
    d : PMap[Expr, Index] = field(default_factory=lambda: pmap())

    def __bool__(self) -> bool:
        return True

    @classmethod
    def make_from(cls, *pairs: Tuple[Expr, Expr]) -> Subst:
        e = pmap().evolver()  # type: ignore[var-annotated]
        for k, v in pairs:
            e[k] = v
        return cls(e.persistent())

    def merge(self, other: Subst) -> Subst:
        return Subst(self.d.update(other.d))

    def __contains__(self, x: Hashable) -> bool:
        return x in self.d

    def simplify(self, expr: Expr) -> Union[Expr, None]:
        match expr:
            case int():
                return expr
            case Plus():
                return expr.value_of(self)
            case _:
                match self.d.get(expr, None):
                    case None:
                        return None
                    case v:
                        return self.simplify(v)

    def as_index(self, expr: Expr) -> Index:
        '''Same as .simplify() but Fizzles if the result is not an Index.'''
        result = self.simplify(expr)
        if isinstance(result, int):
            return result
        else:
            raise FizzleNotIndex(expr)

    def unify(self, lhs: Expr, rhs: Expr) -> Subst:
        #print('UNIFY', lhs, rhs)
        match (lhs, rhs):
            case (int(l), int(r)):
                if l == r:
                    return self
                else:
                    return bottom_subst
            case (Variable(), int(r)):
                #print('GOTN', lhs, r)
                if lhs in self.d:
                    if self.d[lhs] == r:
                        return self
                    else:
                        return bottom_subst
                else:
                    return Subst(self.d.set(lhs, r))
            case (Plus(args=(Variable() as v, int(n))), int(r)):
                #print('GOTPP', v, n, r)
                match self.simplify(v):
                    case int(vv):
                        if vv + n == r:
                            return self
                        else:
                            return bottom_subst
                    case None:
                        return Subst(self.d.set(v, r - n))
            case (Variable(), Plus() as rator):
                #print('GOTVP')
                rvalue = rator.value_of(self)
                #if lhs in self.
                return self # TODO
            case (Variable(), SoupRef()):
                if lhs in self.d:
                    if self.d[lhs] == rhs:
                        return self
                    else:
                        return bottom_subst
                else:
                    return Subst(self.d.set(lhs, rhs))
            case (Variable(), (SoupRef(), p)) if (
                    is_painter(p)  # type: ignore[has-type]
            ):
                if lhs in self.d:
                    if self.d[lhs] == rhs:
                        return self
                    else:
                        return bottom_subst
                else:
                    return Subst(self.d.set(lhs, rhs))
                
            case _:
                lo("Can't unify:", lhs, type(lhs), rhs, type(rhs))
                raise NotImplementedError((lhs, rhs))
        assert False, "unify(): should not go past 'match' stmt"
        return self # Needed only to please mypy; stops [return] error

    def short(self) -> str:
        cl = self.__class__.__name__
        items = ', '.join(
            f'{short(k)}={short(v)}' for k, v in self.d.items()
        )
        return f'{cl}({items})'

#    def substitute(self, var: Variable, rhs: Expr) -> Subst:
#        '''Returns a Subst in which every occurence of 'var' has been replaced
#        by 'rhs'.'''
#        return Subst(
#            pmap(
#                (expr_substitute(v, var, rhs), expr_substitute(e, var, rhs))
#                    for v, e in self.d.items()
#        )


class BottomSubst(Subst):
    '''A Subst that maps nothing to nothing and can't unify or substitute
    anything. As a bool, equivalent to False.'''

    def __bool__(self) -> bool:
        return False

    def simplify(self, expr: Expr) -> Union[Index, None]:
        return None

    def unify(self, lhs: Expr, rhs: Expr) -> Subst:
        return self

    def substitute(self, var: Variable, rhs: Expr) -> Subst:
        return self

empty_subst = Subst()
bottom_subst = BottomSubst()
