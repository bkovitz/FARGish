

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
