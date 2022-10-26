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

def run(
    seed: str='a    ',
    lts: List[str]=['ajaqb'],
    asteps: int=40,
    rsteps: int=60,
    fresh: bool=True,  # Create a new Model in global variable 'm'?
    lla: int=0,  # logging level during absorption
    llr: int=2,   # logging level during regeneration
    auto_annotate: Iterable[Annotation]=default_auto_annotations
) -> None:
    global m, last_args
    last_args = dict(
        seed=seed,
        asteps=asteps,
        rsteps=rsteps,
        lla=lla,
        llr=llr,
        auto_annotate=auto_annotate
    )
    if fresh:
        set_log_level(lla)
        m = Model(auto_annotate=auto_annotate)
        lo('INITS', '\n' + m.lts.state_str())
        if asteps:
            for s in lts:
                m.absorb(s, timesteps=asteps)
    set_log_level(llr)
    lo(1, 'LTS', '\n' + m.lts.state_str())
    if rsteps:
        m.regen_from(seed, nsteps=rsteps)
    print(m.canvas)
    print()
    print(m.ws.state_str_with_authors())

def again(**kwargs):
    global last_args
    run(fresh=False, **(last_args | kwargs))

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
        lts=[],
        asteps=0,
        seed='abcabdijk   ',
        rsteps=200,  #500,
        llr=4,
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
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "-r",
        "--rngseed",
        help="random-number seed",
        type=int
    )
    parser.add_argument("--lts", help="the long-term soup", default='ajaqb')
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
    args = parser.parse_args()
    lo('ARGS', args)

    set_rngseed(args.rngseed)
    run(
        seed=args.seed, 
        lts=as_lts(args.lts),
        asteps=args.asteps,
        rsteps=args.rsteps,
        lla=args.lla,
        llr=args.llr
    )

def set_rngseed(rngseed: Optional[int]=None) -> None:
    rngseed = reseed(rngseed)
    lo(0, f'rngseed={rngseed}{newline}')

if __name__ == '__main__':
    parse_and_run()
    #set_rngseed(1)
    #run_ajaqb()
    #run_ajaqb('a    ', ['wxyaaaa'], 120)
    #run('abc   ')
    #run()
    #run_bad()
    #run_test()
    #run_pons()
    #run(lts=['abc'], asteps=100, lla=6, rsteps=0)
