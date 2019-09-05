# util.py -- Generic utility functions and classes for FARGish

from collections.abc import Iterable
import random
import sys

import numpy as np


def is_iter(o):
    return isinstance(o, Iterable)

def as_iter(o):
    if is_iter(o):
        return o
    else:
        return [o]

def reseed(seed=None):
    '''With seed=None: reseeds Python's random-number generator with a new,
    random seed based on the time, and returns it so you can save it.
    Otherwise reseeds Python's random-number generator with seed, and again
    returns seed.'''
    if seed is None:
        random.seed(None)
        seed = random.randrange(sys.maxsize)
    random.seed(seed)
    return seed

def nice_object_repr(self):
    '''Stick  __repr__ = nice_object_repr  inside a class definition and
    repr() will return a nice string for most classes.'''
    items = self.__dict__.items()
    if len(items) == 1:
        return '%s(%s)' % (self.__class__.__name__, next(iter(items))[1])
    elif len(items) == 0:
        return self.__class__.__name__
    else:
        return '%s(%s)' % (self.__class__.__name__,
                           ', '.join('%s=%s' % (k, repr(v))
                                       for k, v in items))

def rescale(xs, new_total=1.0):
    '''Returns list of xs, rescaled to sum to new_total.'''
    multiplier = new_total / sum(xs)
    return [multiplier * x for x in xs]

def sample_without_replacement(items, k=1, weights=None):
    '''k is number of items to choose. If k > len(items), returns only
    len(items) items.'''
    if weights is not None:
        weights = rescale(weights)
        items = [x for (i, x) in enumerate(items) if weights[i] > 0.0]
        weights = [w for w in weights if w > 0.0]
    return np.random.choice(
        items, size=min(k, len(items)), replace=False, p=weights
    )
