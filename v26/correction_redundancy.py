# correction_redundancy.py  -- Obsolete code for the version of RMem that
#                              pads the canvas during with memories that the
#                              new canvas recalls.
#
# The functions in this file should be converted into an RMem mix-in and
# then this file should be deleted. BEN 4-Feb-2022


CanvasPrep = Callable[[Canvas, 'RMem'], Canvas]
    # Function to call before absorbing a Canvas, e.g. to add redundancy.

def ndups(n: int=1) -> CanvasPrep:
    def ndups_f(c: Canvas, rmem: RMem) -> Canvas:
        if isinstance(c, Canvas1D):
            return Canvas1D(c.contents * n)
        else:
            raise NotImplementedError
    return ndups_f

def no_prep(c: Canvas, rmem: RMem) -> Canvas:
    '''An identity function for Canvases.'''
    return c

def pad_tup(tup: Tuple, ndups: int) -> Tuple:
    return tuple(None for _ in range(ndups * len(tup))) + tup

def correction_redundancy(ndups: int=2, npartial: int=2, niters: int=50) \
-> CanvasPrep:
    def correction_redundancy_f(c: Canvas, rmem: RMem) -> Canvas1D:
        if not isinstance(c, Canvas1D):
            raise NotImplementedError
        tup_in = c.as_tuple()
        lo(tup_in)
        tup_out: ValueTup
        if len(rmem) == 0:
           tup_out = pad_tup(tup_in, ndups)
        else:
            lo('TUP_IN', tup_in)
            #pr(rmem.gset)
            relateds: Set[Tuple] = set()
            while len(relateds) < npartial:
                cue = pad_tup(
                    partial_tup(c.as_tuple(), k=npartial),
                    ndups=ndups
                )
                canvas_to_correct = rmem.regenerate(cue, niters=niters)
                t = canvas_to_correct.as_tuple()[-len(tup_in):]
                relateds.add(canvas_to_correct.as_tuple()[-len(tup_in):])
                lo('CC2C', cue, canvas_to_correct, t, len(relateds), npartial)
                #pr(rmem.lsteps)
            tup_out = reduce(operator.add, relateds) + tup_in
        lo(tup_out)
        return Canvas1D(list(tup_out))
    return correction_redundancy_f

def partial_tup(tup: ValueTup, k: int=3) -> ValueTup:
    r = range(len(tup))
    addrs = set(sample_without_replacement(r, k=k))
    return tuple(
        tup[a] if a in addrs else None
            for a in r
    )

