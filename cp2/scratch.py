# Pass a Canvas to to_detaddrs() instead of a Model? Or maybe pass a new
# kind of object: a Context (holds Model, Canvas, primitive_funcs, whatever,
# and is capable of providing overrides to narrow a search). Maybe call it
# a SearchableContext.

class Addr(ABC):

    @abstractmethod
    def to_detaddrs(self, model: Model, subst: Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        pass

class DetAddr(Addr, ABC):
    pass

class Func(ABC):

    @abstractmethod
    def apply(self, model: MM.Model, subst: SM.Subst, value: Value) \
    -> Value:
        pass

    @abstractmethod
    def can_make(self, model: MM.Model, subst: SM.Subst) -> bool:
        pass

@dataclass(frozen=True)
class Letter(Func)
    c: str

    def apply(self, model: MM.Model, subst: SM.Subst, value: Value) \
    -> Value:
        return c

    def can_make(self, model: MM.Model, subst: SM.Subst) -> bool:
        return True

@dataclass(frozen=True)
class Index(DetAddr, Func):

    def to_detaddrs(self, model: Model, subst: Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        yield self

    def apply(self, model: MM.Model, subst: SM.Subst, value: Value) \
    -> Value:
        return self

    def can_make(self, model: MM.Model, subst: SM.Subst) -> bool:
        return True

@dataclass(frozen=True)
class Indices(DetAddr):

    def to_detaddrs(self, model: Model, subst: Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        yield self

    def apply(self, model: MM.Model, subst: SM.Subst, value: Value) \
    -> Value:
        return self

    def can_make(self, model: MM.Model, subst: SM.Subst) -> bool:
        return True

@dataclass(frozen=True)
class Variable(Addr):

    def to_detaddrs(self, model: Model, subst: Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        (copy)

@dataclass(frozen=True)
class RelatedPair(Addr):
    i: Addr
    j: Addr
    f: Func

    def to_detaddrs(self, model: Model, subst: Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        (copy but need primitive funcs; might need to unify var)

@dataclass(frozen=True)
class MatchContent(Addr):
    content: CellContent

    def to_detaddrs(self, model: Model, subst: Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        yield from (
            DetAddrWithSubst(subst.unify(var, index), index)
                for index in model.canvas.add_matching_indices(self.content)
        )

@dataclass(frozen=True)
class Variable(Addr):
    name: str

    def to_detaddrs(self, model: Model, subst: Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        if self in subst:
            yield from (
                self.addr_to_detaddrs(subst, var, subst.simplify(self))
            )
        else:
            yield from (
                DetAddrWithSubst(
                    subst.unify(var, index).unify(self, index),
                    index
                )
                    for index in model.canvas.all_addrs()
            )

@dataclass(frozen=True)
class Plus(Addr):
    args: Tuple[Expr, ...]

    def to_detaddrs(self, model: Model, subst: Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        match subst.as_index(self):
            case None:
                return
            case Index() as i | Indices() as i:
                yield DetAddrWithSubst(subst.unify(var, i), i)
            case x:
                raise NotImplementedError(f"Can't match Plus that simplifies to {x}, {type(x)}")

@dataclass(frozen=True)
class SoupRef(Addr):
    name: str

    def to_detaddrs(self, model: Model, subst: Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        yield DetAddrWithSubst(subst, addr)  # Don't unify var with anything

@dataclass(frozen=True)
class Painter(Addr, Func):

    def to_detaddrs(self, model: Model, subst: Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        for painter in model.soups():
            subst2 = subst.unify(self, painter)
            if subst2:
                yield DetAddrWithSubst(subst2, painter)

    def to_detpainters(self, model: Model) -> Iterable[DetPainter]:
        (copy this)

    def apply(self, model: MM.Model, subst: SM.Subst, value: Value) \
    -> Value:
        return self

@dataclass(frozen=True)
class CPainter(Painter):
    '''A canvas-painter.'''
    pass

@dataclass(frozen=True)
class PPainter(Painter):
    '''A painter-painter.'''
    pass

Value = Union[Letter, Painter, Func]
Expr = Union[Addr, Func, None]  # what about (SoupRef, Painter)?
