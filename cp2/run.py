import sys

from Model import Model
from Log import lo, trace, indent_log, set_log_level
from util import reseed, short, pts, nf


m: Model

def run_test() -> None:
    global m
    m = Model.canvas_from('ajaqb')
    print(m.lts)
    m.absorb('ajaqb')


if __name__ == '__main__':
    if len(sys.argv) < 2:
        #seed = 4993487641984628738  #None
        seed = None
    else:
        seed = int(sys.argv[1])
    seed = reseed(seed)
    print(f'seed={seed}')
    print()

    run_test()
    #run_ajaqb('a    ', ['wxyaaaa'], 120)
    #run_abs()
