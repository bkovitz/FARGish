import sys

from Model import *
from Log import lo, trace, indent_log, set_log_level
from util import newline, nf, reseed, pts, short


m: Model

def run_ajaqb(
    seed: str='a    ',
    lts: List[str]=['ajaqb'],
    asteps: int=40,
    rsteps: int=60
) -> None:
    global m
    m = Model.canvas_from('ajaqb')
    #print(m.lts)
    m.absorb('ajaqb', timesteps=asteps)
    set_log_level(2)
    m.regen_from(seed, nsteps=rsteps)
    print(m.canvas)


if __name__ == '__main__':
    if len(sys.argv) < 2:
        #seed = 1  # never fills in cell 4
        #seed = 3  # Fizzle
        seed = None
    else:
        seed = int(sys.argv[1])
    seed = reseed(seed)
    lo(0, f'seed={seed}{newline}')

    run_ajaqb()
    #run_ajaqb('a    ', ['wxyaaaa'], 120)
    #run_abs()
