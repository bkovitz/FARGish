# plots.py -- utility functions for plotting functions

import csv
from collections import defaultdict
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence

import matplotlib.pyplot as plt
import numpy as np


def plotf(f: Callable, lb: float=0.0, ub: float=10.0):
    '''Plots a function on the screen.'''
    xs = np.arange(lb, ub, 0.05)
    ys = [f(x) for x in xs]
    #plt.ion()
    plt.plot(xs, ys)
    plt.show()
