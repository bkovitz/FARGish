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

from Types import Addr, Expr, F, Func, I, Index, Indices, J, \
    FizzleNotIndex, SimpleFunc, SoupRef, Variable, \
    as_index, is_func, is_index, is_painter
import Model as MM
import Funcs as FM
from Addrs import RelatedPair, SpecialAddr
from Log import lo, trace, indent_log
from util import force_setattr, short


class HasAsIndex(ABC):

    @abstractmethod
    def as_index(self) -> Index:
        pass

@dataclass(frozen=True)
class Plus(HasAsIndex):
    args: Tuple[Expr, ...]

    def __init__(self, *args: Expr):
        force_setattr(self, 'args', args)

    def __repr__(self) -> str:
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
#        e = pmap().evolver()  # type: ignore[var-annotated]
#        for k, v in pairs:
#            e[k] = v
#        return cls(e.persistent())
        result = cls()
        for lhs, rhs in pairs:
            result = result.unify(lhs, rhs)
        return result

    def merge(self, other: Subst) -> Subst:
        return Subst(self.d.update(other.d))

    def __contains__(self, x: Hashable) -> bool:
        return x in self.d

    def __getitem__(self, x: Hashable) -> Optional[Index]:
        return self.d.get(x, None)

    def simplify(self, expr: Expr) -> Expr:
        match expr:
            case int():
                return expr
            case Plus():
                return expr.value_of(self)
            case _:
                match self.d.get(expr, None):
                    case None:
                        return expr
                    case v:
                        return self.simplify(v)

#    # TODO UT
#    def simplify_as_funcs(self, primitive_funcs: Iterable[Func], func: Func) \
#    -> Iterable[Func]:
#        match func:
#            case Variable():
#                yield
        
    # TODO rm?
    def as_index_fizzle(self, expr: Expr) -> Index:
        '''Same as .simplify() but Fizzles if the result is not an Index.'''
        result = self.simplify(expr)
        if isinstance(result, int):
            return result
        else:
            raise FizzleNotIndex(expr)

    def as_index(self, expr: Expr) -> Optional[Index]:
        '''Same as .simplify() but returns None if the result is not an
        Index.'''
        result = self.simplify(expr)
        if is_index(result):
            return result
        else:
            return None

    def unify(self, lhs: Expr, rhs: Expr) -> Subst:
        with indent_log(8, 'UNIFY', lhs, rhs):
            match (lhs, rhs):
                case (x, y) if x == y:
                    return self
                case (x, Plus(args)) | (Plus(args), x) if len(args) == 1:
                    return self.unify(x, args[0])
                case (Variable(), int(r)):
                    return self.substitute(lhs, rhs).set_lhs_rhs(lhs, rhs)
                case (Variable(), Indices()):
                    return self.set_lhs_rhs(lhs, rhs)
                    # WRONG Need to simplify the elements of Indices
                    # And need to substitute().
                case (int(), int()):
                    return bottom_subst  # we already know they're not equal
                case (Indices(), Indices()):
                    return bottom_subst  # we already know they're not equal
                case (int(), Indices()):
                    return self if Indices(lhs) == rhs else bottom_subst
                case (Indices(), int()):
                    return self if lhs == Indices(rhs) else bottom_subst
                case (int(), Plus()):
                    return self.unify(rhs, lhs)
                case (Plus(args=(Variable() as v, int(n))), int(r)):
                    lo(9, 'GOTPP', v, n, r)
                    match self.simplify(v):
                        case int(vv):
                            return self.unify(vv + n, r)
                        case Variable() as var:
                            return self.unify(var, r - n)
                        case None: # Is this still necessary?
                            return self.set_lhs_rhs(v, r - n)
                            #return Subst(self.d.set(v, r - n))
                        case _:
                            lo("Unimplemented Plus() unification:", lhs, type(lhs), ' with ', rhs, type(rhs), self.simplify(v))
                            raise NotImplementedError((lhs, rhs))
                case (Variable(), Plus() as rator):
                    #print('GOTVP')
                    rvalue = rator.value_of(self)
                    #if lhs in self.
                    return self # TODO
                case (Variable(), Variable()) | (Plus(), Plus()):
                    lhsimple = self.simplify(lhs)
                    rhsimple = self.simplify(rhs)
                    if lhsimple != lhs or rhsimple != rhs:
                        return self.unify(lhsimple, rhsimple)
                        # Could this make an infinite loop?
                    else:
                        return self.set_lhs_rhs(lhs, rhs)
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
                case (Variable(), (i, j, f)):
                    (ii, jj, ff) = (
                        self.simplify(i),
                        self.simplify(j),
                        self.simplify(f)
                    )
                    if (
                        occurs_in(lhs, ii)
                        or
                        occurs_in(lhs, jj)
                        or
                        occurs_in(lhs, ff)
                    ):
                        return bottom_subst
                    else:
                        return self.set_lhs_rhs(lhs, (ii, jj, ff))
                        # Is that wrong? What if ii, etc. have an expr that
                        # needs to be evaluated later?
                case (Variable(), f) if is_func(f):
                    if lhs in self.d:
                        if self.d[lhs] == rhs:
                            return self
                        else:
                            return bottom_subst
                    else:
                        return Subst(self.d.set(lhs, rhs))
                case (Variable(), SpecialAddr()):
                    return bottom_subst
                case (Variable(), _):
                    return bottom_subst
                case (SimpleFunc(var), rhs):
                    if isinstance(rhs, FM.SimpleFuncClass):
                        return self.set_lhs_rhs(var, rhs)
                    else:
                        return bottom_subst
                case (f, g) if callable(f) and callable(g):
                    if f == g:
                        return self
                    else:
                        return bottom_subst
                case (f, _) if callable(f):  # BUG If _ is var, should unify
                    return bottom_subst
                case ((i1, j1, f1), (i2, j2, f2)): # unify two painters
                    # We define I, J, F to be i2, j2, f2 unless I, J, F
                    # get unified in the course of unifying i1 with i2, etc.
                    result = self.unify(i1, i2)
                    if I not in result:
                        result = result.unify(I, i2)

                    result = result.unify(j1, j2)
                    if J not in result:
                        result = result.unify(J, j2)

                    result = result.unify(f1, f2)
                    if F not in result:
                        result = result.unify(F, f2)
                    return result
                case (x, Variable()):
                    return self.unify(rhs, x)
                case (x, SoupRef()):
                    return bottom_subst
                case _:
                    lo("Unimplemented unification:", lhs, type(lhs), ' with ', rhs, type(rhs))
                    raise NotImplementedError((lhs, rhs))
            assert False, "unify(): should not go past 'match' stmt"
            return self # Needed only to please mypy; stops [return] error

    def set_lhs_rhs(self, lhs: Expr, rhs: Expr) -> Subst:
        if lhs in self.d:
            if self.d[lhs] == rhs:
                return self
            else:
                return bottom_subst
        else:
            return Subst(self.d.set(lhs, rhs))

    def unify_if_undefined(self, lhs: Expr, rhs: Expr) -> Subst:
        if lhs not in self:
            return self.unify(lhs, rhs)
        else:
            return self

    def unify_ijf(self, source: Addr, target: Addr, func: Func) -> Subst:
        return self. \
            unify_if_undefined(I, source). \
            unify_if_undefined(J, target). \
            unify_if_undefined(F, func)

    def short(self) -> str:
        cl = self.__class__.__name__
        items = ', '.join(
            f'{short(k)}={short(v)}' for k, v in self.d.items()
        )
        return f'{cl}({items})'

    def substitute(self, lhs: Expr, rhs: Expr) -> Subst:
        '''Returns a Subst in which every occurence of 'lhs' has been replaced
        by 'rhs'.'''
        result = empty_subst
        for l, r in self.d.items():
            result = result.unify(
                expr_substitute(l, lhs, rhs),
                expr_substitute(r, lhs, rhs)
            )
            if not result:
                return bottom_subst
        return result

def expr_substitute(e: Expr, lhs: Expr, rhs: Expr) -> Expr:
    '''Returns a new Expr consisting of e where every occurrence of lhs has
    been replaced by rhs.'''
    match e:
        case x if x == lhs:
            return rhs
        case Plus(args):
            return Plus(*(expr_substitute(a, lhs, rhs) for a in args))
        case (i, j, f):
            return (
                expr_substitute(i, lhs, rhs),
                expr_substitute(j, lhs, rhs), 
                expr_substitute(f, lhs, rhs)
            )
        # TODO UT
        case RelatedPair(i, j, f):
            return RelatedPair(
                expr_substitute(i, lhs, rhs),
                expr_substitute(j, lhs, rhs), 
                expr_substitute(f, lhs, rhs)
            )
        # TODO UT
        case FM.MakeBetweenPainter(i, j, f):
            return FM.MakeBetweenPainter(
                expr_substitute(i, lhs, rhs),
                expr_substitute(j, lhs, rhs), 
                expr_substitute(f, lhs, rhs)
            )
        # TODO UT
        case FM.MakeRelativeIndirectPainter(i, j, f):
            return FM.MakeRelativeIndirectPainter(
                expr_substitute(i, lhs, rhs),
                expr_substitute(j, lhs, rhs), 
                expr_substitute(f, lhs, rhs)
            )
        case _:  # no match; nothing to substitute
            return e

def occurs_in(var: Variable, expr: Expr) -> bool:
    match expr:
        case Variable():
            return var == expr
        case (i, j, f):
            return occurs_in(var, i) or occurs_in(var, j) or occurs_in(var, f)
        case Plus(args):
            return any(occurs_in(var, a) for a in args)
        case RelatedPair(i, j, f):
            return occurs_in(var, i) or occurs_in(var, j) or occurs_in(var, f)
        case FM.MakeBetweenPainter(i, j, f):
            return occurs_in(var, i) or occurs_in(var, j) or occurs_in(var, f)
        case FM.MakeRelativeIndirectPainter(i, j, f):
            return occurs_in(var, i) or occurs_in(var, j) or occurs_in(var, f)
        case _:
            return False

class BottomSubst(Subst):
    '''A Subst that maps nothing to nothing and can't unify or substitute
    anything. As a bool, equivalent to False.'''

    def __bool__(self) -> bool:
        return False

    def simplify(self, expr: Expr) -> Union[Index, None]:
        return None

    def unify(self, lhs: Expr, rhs: Expr) -> Subst:
        return self

    def set_lhs_rhs(self, lhs: Expr, rhs: Expr) -> Subst:
        return self

    def substitute(self, var: Variable, rhs: Expr) -> Subst:
        return self

empty_subst = Subst()
bottom_subst = BottomSubst()
