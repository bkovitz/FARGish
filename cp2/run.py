import sys

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
    llr: int=2   # logging level during regeneration
) -> None:
    global m, last_args
    last_args = dict(
        seed=seed,
        asteps=asteps,
        rsteps=rsteps,
        lla=lla,
        llr=llr
    )
    if fresh:
        set_log_level(lla)
        m = Model()
        for s in lts:
            m.absorb(s, timesteps=asteps)
    set_log_level(llr)
    lo(1, 'LTS', m.lts.state_str())
    m.regen_from(seed, nsteps=rsteps)
    print(m.canvas)
    print()
    print(m.ws.state_str_with_authors())

def again(**kwargs):
    global last_args
    run(fresh=False, **(last_args | kwargs))

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
    run('abc   ')
