from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check
import sys

import matplotlib.pyplot as plt

from Model import *
from Log import lo, trace, indent_log, set_log_level
from util import newline, nf, reseed, pts, short


m: Model
last_args: Dict[str, Any] = {}

#def run_ajaqb(
#    seed: str='a    ',
#    lts: List[str]=['ajaqb'],
#    asteps: int=40,
#    rsteps: int=60
#) -> None:
#    global m
#    m = Model.canvas_from('ajaqb')
#    #print(m.lts)
#    set_log_level(6)
#    m.absorb('ajaqb', timesteps=asteps)
#    return
#    lo('LTS', m.lts.state_str())
#    set_log_level(2)
#    m.regen_from(seed, nsteps=rsteps)
#    print(m.canvas)
#    print()
#    print(m.ws.state_str_with_authors())

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

if __name__ == '__main__':
    if len(sys.argv) < 2:
        #seed = 1  # never fills in cell 4
        #seed = 3  # Fizzle
        seed = None
    else:
        seed = int(sys.argv[1])
    seed = reseed(seed)
    lo(0, f'seed={seed}{newline}')

    #run_ajaqb()
    #run_ajaqb('a    ', ['wxyaaaa'], 120)
    #run('abc   ')
    #run()
    run_bad()
