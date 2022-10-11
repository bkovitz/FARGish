import sys

from Model import Model
from Funcs import *
from Subst import *
from Painters import *
from Log import lo, trace, indent_log, set_log_level
from util import newline, nf, reseed, pts, short


m: Model

def run_test() -> None:
    global m
    m = Model.canvas_from('ajaqb')
    #print(m.lts)
    m.absorb('ajaqb', timesteps=40)
    set_log_level(1)
    m.regen_from('a    ', nsteps=120)


if __name__ == '__main__':
    if len(sys.argv) < 2:
        #seed = 1  # never fills in cell 4
        #seed = 3  # Fizzle
        seed = None
    else:
        seed = int(sys.argv[1])
    seed = reseed(seed)
    lo(1, f'seed={seed}{newline}')

    run_test()
    #run_ajaqb('a    ', ['wxyaaaa'], 120)
    #run_abs()
