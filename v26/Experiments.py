# Experiments.py  --  Configured runs of RMem

from RMem import RMem
from util import pts, pr, ps, pss, psa, pl


rmem: RMem = None

def exp1():
    global rmem
    rmem = RMem.run(
        operands=range(1, 4),
        startc=(None, '+', 1, None, 3),
        ndups=2,
        niters=1000
    )
    pr(rmem.lsteps)
    
