

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
    
