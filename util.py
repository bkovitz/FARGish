# util.py -- Generic utility functions and classes for FARGish

from collections.abc import Iterable
import random
import sys


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


