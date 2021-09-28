# Numbo1a.py -- Numbo1.py revised to import FARGModel instead of FARGish2

from dataclasses import dataclass, field, replace, asdict
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence, Literal

import matplotlib.pyplot as plt  # type: ignore[import]
import numpy as np  # type: ignore[import]

from FARGModel import FARGModel, SeqCanvas, Want
from FMTypes import epsilon, Elem, Elems, Value, Addr, FMPred
from util import pts, pl, pr


class Numbo(FARGModel):
    pass

def r4_5_6__15(*args, **kwargs):
    global fm, ca, wa
    fm = Numbo(*args, **kwargs)
    ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
    wa = fm.build(Want(15, canvas=ca, addr=0))
    fm.do_timestep(num=19)
    pr(fm, edges=True)
    print()
    #fm.pr_flows()
    print(f'seed={fm.seed}')
    
