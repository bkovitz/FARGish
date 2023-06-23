from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check
import sys
import argparse

import matplotlib.pyplot as plt

from Model import *
from Log import lo, trace, indent_log, set_log_level
from util import newline, nf, reseed, pts, short


m: Model
last_args: Dict[str, Any] = {}
rngseed: Optional[int] = None

# NEXT At the end of a run, print out all the global parameters in a 
# concise form.

def run(
    seed: str='a    ',
    ltm: List[str]=['ajaqb'],
    asteps: int=100,   #40,
    rsteps: int=60,   #60,
    fresh: bool=True,  # Create a new Model in global variable 'm'?
    lla: int=0,  # logging level during absorption
    llr: int=2,   # logging level during regeneration
    auto_annotate: Iterable[Annotation]=default_auto_annotations,
    ab: List[Painter]=default_initial_painters,
    abr: bool=True,   # allow ab initio painters during regeneration?
    rng: Optional[int]=None, # random-number seed, or None to randomize
    exc: bool=False,  # exclude absolute painters from lts?
    ccl: bool=True,   # allow source/target cell clarity to affect probability?
    pcl: bool=False,  # allow painter clarity to affect probability?
    pun: bool=False # allow punishing of painters for overwriting given letters?
) -> None:
    global m, last_args
    last_args = dict(
        seed=seed,
        asteps=asteps,
        rsteps=rsteps,
        lla=lla,
        llr=llr,
        auto_annotate=auto_annotate,
        ab=ab,
        exc=exc
    )
    if fresh:
        set_rngseed(rng)
        set_global_param('allow_ab_initio_painters', True)
        set_log_level(lla)
        m = Model(
            lts=Soup.make_from(ab),
            auto_annotate=auto_annotate,
            exclude_abs=exc
        )
        lo(1, 'INITS', '\n' + m.lts.state_str())
        if asteps:
            for s in ltm:
                m.absorb(s, timesteps=asteps)
    set_global_param('punishing', pun)
    set_global_param('painter_clarity', pcl)
    set_global_param('cell_clarity', ccl)
    set_log_level(llr)
    lo(1, 'LTS\n' + m.lts.state_str())
    #lo(1, 'LTS\n' + m.lts.state_str_with_authors())
    if rsteps:
        set_global_param('allow_ab_initio_painters', abr)
        m.regen_from(seed, nsteps=rsteps)
        set_global_param('allow_ab_initio_painters', True)
    print(m.canvas)
    print()
    print(m.ws.state_str_with_authors())

def again(**kwargs):
    global last_args
    run(fresh=False, **(last_args | kwargs))

def run1(**kwargs):
    '''1st example in dissertation.'''
    set_latex_mode()
    run(ab=[ab1, ab3], **kwargs)


def run_bad(**kwargs) -> None:
    d = dict(
        auto_annotate=[Start, End],
        asteps=100,
        rsteps=0,
        lla=4
    )
    run(**(d | kwargs))

def run_test(**kwargs) -> None:
    d = dict(
        auto_annotate=[Start, End],
        asteps=100,
        rsteps=60,
        lla=6,
        llr=6,
        seed='m    '
    )
    run(**(d | kwargs))

def run_pons(**kwargs) -> None:
    '''Runs the pons asinorum.'''
    lo(0, "pons asinorum")
    d = dict(
        ltm=[],
        asteps=0,
        seed='abcabdijk   ',
        rsteps=200,  #500,
        #llr=4,
        auto_annotate=[]
    )
    run(**(d | kwargs))

def h(*ids):
    '''Plot history of painters with given ids.'''
    global m
    for i in ids:
        #plt.plot(range(1, m.t + 1), m.history_of(i))
        #plt.plot(*zip(m.history_of(i)))
        hs = list(zip(*m.history_of(i)))
        pts(hs)
        print(len(hs))
        plt.plot(*hs)
    plt.show()

def as_lts(s: str) -> List[str]:
    if not s:
        return []
    elif ',' not in s:
        return [s]
    else:
        return s.split(',')

def parse_and_run() -> None:
    global rngseed
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "-r",
        "--rngseed",
        help="random-number seed",
        type=int
    )
    parser.add_argument("--ltm", help="the long-term soup", default='ajaqb')
    parser.add_argument(
        "seed",
        help="the seed string",
        default='a    ',
        nargs='?'
    )
    parser.add_argument(
        "--asteps",
        help="number of timesteps to absorb each LTS string",
        type=int,
        default=40
    )
    parser.add_argument(
        "--rsteps",
        help="number of timesteps to regenerate",
        type=int,
        default=60
    )
    parser.add_argument(
        "--lla",
        help="logging level during absorption",
        type=int,
        default=0
    )
    parser.add_argument(
        "--llr",
        help="logging level during regeneration",
        type=int,
        default=2
    )
    parser.add_argument(
        "--au",
        help="which set of cell attributes to auto-annotate",
        type=int,
        choices=[0, 1],
        default=global_params.auto_annotations
    )
    args = parser.parse_args()

    global_params.auto_annotations = args.au
    run(
        seed=args.seed, 
        ltm=as_lts(args.ltm),
        asteps=args.asteps,
        rsteps=args.rsteps,
        lla=args.lla,
        llr=args.llr,
        rng=args.rngseed
    )
    lo(0, f'rngseed={rngseed}{newline}')

def set_rngseed(r: Optional[int]=None) -> None:
    global rngseed
    rngseed = reseed(r)
    lo(0, f'rngseed={rngseed}{newline}')

def runabs1():
    # abs painters only
    run(seed='a    ', ltm=['abcde'], ab=[ab1])

def runabs2():
    # abs painters with a different seed
    run(seed='m    ', ltm=['abcde'], ab=[ab1])

def runabs_ajaqb(**kwargs):
    run(seed='a    ', ltm=['ajaqb'], ab=[ab1])
    # no reln with j or q

def runab13_ajaqb(seed='a    '):
    run(seed, ltm=['ajaqb'], ab=[ab1, ab3])

def runabs3():
    # abs painters with more LTM
    run(seed='a    ', ltm=['abcde', 'ggggg'], ab=[ab1])
    # problem: many sames => many nonadjacent 'same' painters
    # therefore 'aaaaa' wins



def runrel():
    kw = dict(
        seed='a    ',
        ltm=['abcde'],
        ab=[ab1, ab2],
        asteps=100,
        rsteps=40,
        exc=True
    )
    run(**kw)

def runab123(**kwargs):
    kw = dict(ltm=['ajaqb'], ab=[ab1, ab2, ab3], asteps=100, exc=True) | kwargs
    run(**kw)

# NEXT Try absolute & digraph painters with a big LTM to see if that creates
# a need for clarity.

def run_ad(**kwargs):
    # make relative (digraph) painters: gets 'aqb  ' a lot
    kw = dict(
        #ltm=['ajaqb', 'pqrst', 'abcde', 'aabbc'],
        #ltm=['ajaqb', 'abcde', 'aabbc'],
        #ltm=['ajaqb', 'abcde'],
        ltm=['ajaqb'],
        ab=[ab4],
        asteps=30,
        rsteps=25,
        lla=0,
        llr=1,
        pun=False,
        exc=True
    ) | kwargs
    run(**kw)

def run_ad2(**kwargs):
    # blends some simple strings
    kw = dict(ltm=['ajaqb', 'abcde', 'aabbc']) | kwargs
    run(**kw)

def r(d: dict, **kwargs):
    '''Convenience function for running experiments in the REPL. Optionally
    override a dict of arguments to run().'''
    run(**(d | kwargs))

# A run just like the Hopfield net: many strings in memory, absolute painters
# only.
hoplike = dict(
    ltm=['ajaqb', 'pqrst', 'abcde', 'aabbc', 'ponop'],
    ab=[ab1],
    asteps=40,
    rsteps=100,
    lla=0,
    llr=2,
    pun=False,
    pcl=False,
    exc=False
)

hoplike_long = hoplike | dict(
    ltm=['abrsecpbqf', 'efghijklmn', 'tfserfdqgc', 'abcdepomnl'],
    seed='abr e  bqf'
)

hoplike_long_easy = hoplike | dict(
    ltm=['aaaaabca', 'gggvvwgg', 'pqrspqrs'],
    seed='aaaa   a',
    ab=[ab1a],
    rsteps=30,
    ccl=False
)

example1 = hoplike_long_easy | dict(
    #seed='aa     a',
    seed='  aabb  ',
    rsteps=20,  #50,
    abr=False,
    ccl=False,
    rng=444834040015719226,  #8066335492150159463,
    llr=2
)

example2 = example1 | dict(
    ccl=True,
    rsteps=40,
)

rel0 = dict(   # reconstructs with abs painters only, because seed is aligned
               # right
    ltm=['bbbbghij'],
    seed='b   g   ',
    ab=[ab1a],
    asteps=150,
    rsteps=50,
    abr=False,
    pcl=False,
    llr=2
)

rel1 = rel0 | dict(  # shows inability of abs painters to adjust to a shift (??)
    seed='b  gh   ',
    rsteps=50,
    rng=620217766481971979
)
# PROBLEM This actually generates the desired analogy! bbbghijk
# We need an illustration of how absolute painters can't follow a shift.

rel2 = rel1 | dict(  # relative painters: a bit nicer
    ab=[ab4]
)


rel0a = rel0 | dict(
    ltm=['bbbbghib'],
    seed='b   g   '
)

rel1a = rel0a | dict(
    seed='b g     ',
)

rel2a = rel1a | dict(
    ab=[ab4],
    rsteps=120,
    rng=682026381905476632
)

# Can we solve 'ajaqb' without relative indirect painters?
quest1 = dict(
    ltm=['ajaqb'],
    seed='a    ',
    asteps=40,
    rsteps=40,
    ab=[ab1, ab3],
    pun=False,
    exc=True
)
# No.
# seed='a a  ' also fails, because without absolute or relative indirect
# painters, the LTS has no record of the a_a and a_b relationships.
# A relative indirect painter can recreate that relationship wherever it
# sees an 'a' or 'b'.

# Does adding relative indirect solve that problem?
quest2 = quest1 | dict(
    ab=[ab1, ab2, ab3]
)

# Does adding relative painters get hoplike_long to regenerate the memories
# reliably?
quest3 = hoplike_long | dict(
    ab=[ab1, ab4]
)

# Is clarity needed to settle down on an attractor?
quest4 = hoplike_long | dict(
    ccl=False
)

cdecb = dict(
    seed='  e  ',
    ltm=['cdecb'],
    ab=[ab1a],
    asteps=30,
    ccl=False,
    pcl=False,
    pun=False
)

# hoplike_few

#      pcl=False => ignore painter clarity
#      ccl=False => ignore cell clarity
# TODO rsteps=None => run until all clarities >= 4
#      ann=False => no cell annotations

# TODO Collect together some named experiments that show each point in
# sequence. Start without cell clarity.

ri1 = dict(
    seed='m    ',
    ltm=['ajaqb'],
    ab=[ab1a, ab4],
    abr=False,
    ccl=True,
    pcl=False,
    pun=False,
)

# NEXT Who solves ajaqb with m____? What is the minimum set of ab initio
# painters?
#  Answer: r(ri1, seed='m    ', ab=[ab1a, ab3])

# THEN Find the bug that makes the betw painter match a painter in the LTS.
# r(ri1, ltm=['aaajaqb'], seed='mj     ', ab=[ab1a, a b3, ab4])

# Could we set up a problem where the presence of absolute painters
# interferes with a good solution?


# Can we see any use for the relative indirect painter at all, which we cab
# illustrate with an example?

# Yes. This fails:
# r(ri1, seed='a    ', exc=True, abr=True, ab=[ab1a, ab3])
# This succeeds:
# r(ri1, seed='a    ', exc=True, abr=True, ab=[ab1a, ab2, ab3])


# To illustrate Process A and Process B:
#    Need abr=True to be necessary to solve problem with some set of
#    ab initio painters.

#  ((I, I+2, same), ws, (I, I+4, succ))   <-- this would do it, even on m____

src = Painter(I, Plus(I, 2), same)
fun = Painter(I, Plus(I, 4), succ)
p1 = Painter(src, SR.WorkingSoup, fun)

# Fails:
# r(ri1, seed='m m  ', exc=True, abr=False, ab=[ab1a, ab3, p1])
# Succeeds:
# r(ri1, seed='m m  ', exc=True, abr=True, ab=[ab1a, ab3, p1])

# New ab initio painter:
#  See two abs painters with one overlapping index, make the above painter.

# DECISION: Omit relative indirect painter, include only this painter above.

if __name__ == '__main__':
    #parse_and_run()  # Uncomment this to get normal command line

    #run_ad(pun=True, llr=5, rsteps=2)
    #r(hoplike, seed='a  de')   # erratic

    #r(hoplike_long)
    #'ghijk    '

    #r(hoplike_long_easy)
    #r(example1)
    #r(rel1)
    r(ri1)

    #r(cdecb, llr=2, rsteps=0, lla=2)

    #set_rngseed(1)
    #run_ajaqb()
    #run_ajaqb('a    ', ['wxyaaaa'], 120)
    #run('abc   ')
    #run()
    #run_bad()
    #run_test()
    #run_pons()
    #run(ltm=['ajaqb'], asteps=100, lla=6, rsteps=0)

# WANT Run an experiment with same params as example2, but with hundreds of
# different randomly chosen 4-letter seeds (with 4 randomly placed blanks),
# and see if there are only a small number of attractors. Run to completion
# rather than running a set number of timesteps.
