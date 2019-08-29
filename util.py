# util.py -- Generic utility functions and classes for FARGish

from collections.abc import Iterable


def is_iter(o):
    return isinstance(o, Iterable)

def as_iter(o):
    if is_iter(o):
        return o
    else:
        return list(o)

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


