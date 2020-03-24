# util.py -- Generic utility functions and classes for FARGish

from collections.abc import Iterable
import random
import sys
from inspect import isclass


empty_set = frozenset()
newline = '\n'

def is_iter(o):
    return (
        isinstance(o, Iterable)
        and
        not isinstance(o, str)
        and
        not is_namedtuple(o)
    )

def is_namedtuple(o):
    #HACK
    return hasattr(o, '_fields')

def as_iter(o):
    '''Returns o in a form that the caller can iterate over. If o is already
    an iterable (but not a string), returns o. If o is none, returns an
    empty list. If o is anything else, returns a one-element list containing
    o.'''
    if isinstance(o, str):
        return [o]
    elif is_iter(o):
        return o
    elif o is None:
        return []
    else:
        return [o]

def as_list(o):
    return list(as_iter(o))

def as_set(o):
    '''Converts o to a set if it isn't a set already.'''
    if isinstance(o, set):
        return o
    else:
        return set(as_iter(o))

def as_name(x):
    try:
        return x.name
    except AttributeError:
        return str(x)

def vcat(a, b):
    '''Concatenate value(s). Combines a and b into either a list or a
    single value, preferring the latter. If a is an iterable, modifies a
    and returns it. But if a is None or a non-iterable, vcat creates a
    new list and returns that. So, calling could should look like this:d
      a = vcat(a, b).'''
    if a is None:
        return b
    if b is None:
        return a
    if is_iter(a):
        if is_iter(b):
            a += b
        else:
            a.append(b)
        return a
    if is_iter(b):
        return [a] + b
    return [a, b]
    
def is_nodeid(x):
    return isinstance(x, int)

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
    return repr_str(self.__class__.__name__, self.__dict__.items())

def repr_str(name, items):
    '''items is iterable of (name, value). Returns the string appropriate for
    repr().'''
    if len(items) == 1:
        return '%s(%s)' % (name, nrepr(next(iter(items))[1]))
    elif len(items) == 0:
        return name
    else:
        return '%s(%s)' % (name,
                           ', '.join('%s=%s' % (k, nrepr(v))
                                       for k, v in items))

def nrepr(o):
    '''Helper for nice_object_repr().'''
    if isclass(o):
        return o.__name__
    elif isinstance(o, float):
        return '%.3f' % o
    else:
        return repr(o)

class NiceRepr:
    "Mix-in to give descendants nice_object_repr."
    __repr__ = nice_object_repr

def rescale(xs, new_total=1.0):
    '''Returns list of xs, rescaled to sum to new_total.'''
    if not xs:
        return xs
    s = sum(xs)
    if s == 0:
        x = 1.0 / len(xs)
        return [x] * len(xs)
    else:
        multiplier = new_total / sum(xs)
        return [multiplier * x for x in xs]

# TODO BUG numpy has its own random-number generator. This results in
# indeterminism because other code invokes Python's random-number generator.
#def sample_without_replacement(items, k=1, weights=None):
#    '''k is number of items to choose. If k > len(items), returns only
#    len(items) items.'''
#    if weights is not None:
#        weights = rescale(weights)
#        items = [x for (i, x) in enumerate(items) if weights[i] > 0.0]
#        weights = [w for w in weights if w > 0.0]
#    return np.random.choice(
#        items, size=min(k, len(items)), replace=False, p=weights
#    )

def sample_without_replacement(items, k=1, weights=None):
    '''Returns a generator of k items sampled randomly without replacement from
    'items', weighted by 'weights'. If 'weights' is None, then all items have
    equal probability.  If k > number of items, returns same result as if k =
    number of items.'''
    try:
        n = len(items)
    except TypeError:
        items = list(items)
        n = len(items)
    if weights is None:
        weights = [1.0] * n
    weights = rescale(weights)
    items = [x for (i, x) in enumerate(items) if weights[i] > 0.0]
    weights = [w for w in weights if w > 0.0]
    for i in range(k):
        if items:
            i = random.choices(range(len(items)), weights=weights, k=1)[0]
            item = items.pop(i)
            del weights[i]
            yield item
        else:
            return

def read_to_blank_line(f):
    result = ''
    while True:
        l = f.readline()
        if not l.strip():
            break
        result += l
    return result

def identity(x):
    return x

def filter_none(f, iterable):
    xs = [f(i) for i in iterable if i is not None]
    return [x for x in xs if x is not None]

def intersection(*sets):
    '''Returns a set, which is the intersection of sets. The sets may be
    any iterable, not just 'set' objects.'''
    sets = [
        s if isinstance(s, set) else set(s)
            for s in sets
    ]
    if sets:
        return sets[0].intersection(*sets[1:])
    else:
        return set()

def input_integers(prompt):
    '''Prompts the user to enter a list of integers separated by spaces.
    Returns a list of numbers, or None if user just hit Enter.'''
    while True:
        s = input(prompt)
        if not s:
            return None
        try:
            ns = [int(n) for n in s.split()]
            if not ns:
                continue
            return ns
        except ValueError:
            print('Please enter a list of integers separated by spaces.')
            continue

def setattr_from_kwargs(o, kwargs, *attr_names):
    for attr_name in attr_names:
        try:
            setattr(o, attr_name, kwargs[attr_name])
        except KeyError:
            raise(ValueError(
                f'{o.__class__.__name__} ctor missing argument "{attr_name}".'
            ))

class ReprEq:
    '''Mix-in to make a class's __eq__ and __hash__ work according to the
    output of the class's __repr__.'''

    def __eq__(self, other):
        return repr(self) == repr(other)

    def __hash__(self):
        return hash(repr(self))
