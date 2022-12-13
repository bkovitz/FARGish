

@dataclass(frozen=True)
class Succ:
    i: Arg
    j: Arg

    def unify(self, ctx: Context) -> Iterable[Context]:
        for ii, jj in ctx.all_pairs(self.i, self.j):
            if is_succ(ctx.at(ii), ctx.at(jj)):
                yield ctx.unify(self.i, ii).unify(self.j, jj)
            elif is_blank(ii) and is_filled(jj):
                # WANT unification to fail if at(jj) has no predecessor
                yield ctx.paint(ii, pred_of(ctx.at(jj)))
            elif is_filled(ii) and is_blank(jj):
                yield ctx.paint(jj, succ_of(ii))
            else:
                yield ctx

    def unify(self, ctx: Context) -> Iterable[Context]:
        for ii, jj in ctx.all_addr_pairs(self.i, self.j):
            match (ctx.astatus(ii), ctx.astatus(jj)):
                case (AS.Filled, AS.Filled):
                    yield from ctx.unify(self.i, ii).unify(self.j, jj)
        

@dataclass(frozen=True)
class Apart:
    dist: Arg
    i: Arg
    j: Arg

    def unify(self, ctx: Context) -> Iterable[Context]:
        # how do we deal with dist if it's a Variable?
        for dd, ii, jj in ctx.all_triples(self.dist, self.i, self.j):
            if is_distance(dd, ii, jj):
                yield (
                    ctx.unify(self.dist, dd)
                       .unify(self.i, ii)
                       .unify(self.j, jj)
                )


class AS:
    '''Just a holder for AddrStatuses to make them easy to refer to in 'case'
    statements.'''
    Blank = Atom('Blank')
    Filled = Atom('Filled')
    Nonexistent = Atom('Nonexistent)'

AddrStatus = Literal[AS.Blank, AS.Filled, AS.Nonexistent]


@dataclass(frozen=True)
class Context:
    subst: Subst
    ws: WorkspaceState
    actions: Set[Action]

    def unify(self, e1: Expr, e2: Expr) -> Context:
        pass

    def paint(self, a: Addr, v: Value) -> Context:
        # Return FailedContext if v is None
        pass

    def astatus(self, a: Addr) -> AddrStatus:
        pass

    def all_pairs(self, e1: Expr, e2: Expr) -> \
    Iterable[Tuple[Constant, Constant]]:
        pass

    def all_pairs(self, e1: Expr, e2: Expr, e3: Expr) -> \
    Iterable[Tuple[Constant, Constant, Constant]]:
        pass
    

PainterResult = Tuple[Subst, Action]

@dataclass(frozen=True)
class Painter:
    arguments: Tuple[Argument, Argument]
    predicates: Tuple[Predicate, ...]

    def source_loop(
        self, subst: Subst, us: UState, source_var: Variable
    ) -> Iterable[PainterResult]:

    def to_actions(self, us: UState) -> Iterable[PainterResult]:
        for su in self.loop_from_first_to_second(us, *self.arguments):
            

        for su in self.loop_from_second_to_first(us, *self.reversed_arguments()):

    def reversed_arguments(self) -> Tuple[Argument, Argument]:
        return (self.arguments[1], self.arguments[0])


@dataclass(frozen=True)
class Succ[Predicate]:
    i: Argument
    j: Argument

    def paint_first_given_second(self, us: UState, su: Subst) \
    -> Iterable[PainterResult]:
        yield (su, PaintAt(su.as_addr(self.i), pred_of(us.letter_at(self.j))))

    def paint_second_given_first(self, us: UState, su: Subst) \
    -> Iterable[PainterResult]:
        yield (su, PaintAt(su.as_addr(self.j), pred_of(us.letter_at(self.i))))



p1 = Painter([
    Arguments(I, J),
    Apart(2, I, J),
    Succ(I, J)
])

class Model:

    def painter_to_actions(self, painter: Painter) -> Iterable[Action]:
        for predicate in painter:


@dataclass(frozen=True)
class Condition(ABC):
    
    @abstractmethod
    def is(self, us: UState, o: WorkspaceObject) -> bool:
        pass

@dataclass(frozen=True)
class SuccCond(Condition):
    anchor_var: Variable

    def is(self, us: UState, loop_var: Variable) -> bool:
        return is_succ(us.value_at(
        )
        # Still wrong. Start with Succ.restrict_for_source()

@dataclass(frozen=True)
class Loop:
    variable: Variable
    condition: Condition

    # combine with inner loops in a Loops class
    # restrict the variable, i.e. "unify"
    # somehow extract the condition to build or fill a ws object


# The calling code for the loop(s)

    us = 
    loop = 
    source_var, target_var = 
    for predicate in predicates:
        us, loop = predicate.add_source_loop(us, loop, source_var)
        us, loop = predicate.add_target_loop(us, loop, target_var)
    actions = loop.run(us


class Loops:
    suchthats: Tuple[SuchThat, ...]

class SuchThat:
    var: Variable
    condition: Condition


class Succ:
    i: VarSpec
    j: VarSpec

    def add_source_loop(loop: Loop, source: VarSpec) -> Loop:
        return loop.add_loop_through(source, FilledAddress(source))

    def add_target_loop(loop: Loop, source: VarSpec, target: VarSpec) -> loop:
        return loop.add_loop_through(
            target,
            StartRightOf(source), #SuchThat(target, is_blank),
            action=AsymmetricPaint(self.i, self.j, succ_of, pred_of)



for i_addr in us.filled_addresses():
    for j_addr in us.addrs_to_right_of(i_addr):
        yield PaintAt(j_addr, succ(At(i_addr))



