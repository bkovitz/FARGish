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
rngseed: int

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
    exc: bool=False,  # exclude absolute painters from lts?
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
    set_log_level(llr)
    lo(1, 'LTS\n' + m.lts.state_str())
    #lo(1, 'LTS\n' + m.lts.state_str_with_authors())
    if rsteps:
        m.regen_from(seed, nsteps=rsteps)
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

    set_rngseed(args.rngseed)
    global_params.auto_annotations = args.au
    run(
        seed=args.seed, 
        ltm=as_lts(args.ltm),
        asteps=args.asteps,
        rsteps=args.rsteps,
        lla=args.lla,
        llr=args.llr
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


if __name__ == '__main__':
    #parse_and_run()  # Uncomment this to get normal command line

    run_ad(pun=True, llr=5, rsteps=2)

    #set_rngseed(1)
    #run_ajaqb()
    #run_ajaqb('a    ', ['wxyaaaa'], 120)
    #run('abc   ')
    #run()
    #run_bad()
    #run_test()
    #run_pons()
    #run(ltm=['ajaqb'], asteps=100, lla=6, rsteps=0)
