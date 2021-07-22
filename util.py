# util.py -- Generic utility functions and classes for FARGish

from collections.abc import Iterable
import collections
import random
import sys
from inspect import isclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence
from contextlib import AbstractContextManager
from dataclasses import dataclass, Field, fields
from types import SimpleNamespace
from itertools import chain, tee
import functools


empty_set = frozenset()
newline = '\n'
backslash = '\\'

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

def as_hashable(o) -> Hashable:
    '''Tries to make a Hashable object out of o if it's not. It will probably
    fail.'''
    if isinstance(o, collections.abc.Hashable):
        # Warning: if o is a dataclass with frozen=True, it will be seen as
        # Hashable but it won't be if it contains any unhashable members,
        # like a list or dict.
        return o
    if isinstance(o, list):
        o = tuple(o)
    else:
        raise ValueError(f'{o} cannot be made hashable. Maybe it contains a list.')
    return o

# TODO UT
def omit(d: Dict, keys: Iterable) -> Dict:
    '''Returns copy of 'd' with all members of 'keys' omitted.'''
    return dict(
        (k, v) for k, v in d.items() if k not in as_set(keys)
    )

def d_subset(d: Dict, keys: Iterable) -> Dict:
    '''Returns copy of 'd' with only members of 'keys'; all other items are
    omitted.'''
    return dict(
        (k, v) for k, v in d.items() if k in as_set(keys)
    )

def field_names(dclass) -> List[str]:
    return [f.name for f in fields(dclass)]

def fields_for(dclass, kwargs: Dict[str, Any]) -> Dict[str, Any]:
    return d_subset(kwargs, field_names(dclass))

def force_setattr(obj, attrname, value):
    '''Writes value on obj.attrname even if obj is immutable.'''
    object.__setattr__(obj, attrname, value)

#TODO UT
def loose_dict_eq(d1: Dict, d2: Dict) -> bool:
    '''Are d1 and d2 equal, if we count a value of None as equal to not having
    a value at all?'''
    for k in set(chain(d1.keys(), d2.keys())):
        if d1.get(k, None) != d2.get(k, None):
            return False
    return True

# TODO Rename this to something clearer
def tupdict(**kwargs) -> Tuple[Tuple[str, Hashable]]:
    return tuple(
        (k, v) for k, v in kwargs.items()
    )

def as_dict(x: Union[Dict, None, Collection[Tuple[str, Hashable]]]) -> Dict:
    if isinstance(x, dict):
        return x
    elif x is None:
        return dict()
    else:
        return dict(x)

def as_name(x):
    try:
        return x.name
    except AttributeError:
        return str(x)

def short(x):
    '''Returns a short string representation of x. If x has a .short() method
    define, we call it and return its result. Otherwise we return str(x).'''
    try:
        return x.short()
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

def is_seq_of(x: Any, clas: Type) -> bool:
    if isinstance(x, list) or isinstance(x, tuple):
        try:
            return isinstance(x[0], clas)
        except IndexError:
            return False
    
# TODO rm (OAOO Node.py)
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

def csep(xs) -> str:
    '''Comma-separated string for whatever you pass it.'''
    return ', '.join(str(x) for x in as_iter(xs))

def ssep(xs) -> str:
    '''Space-separated string for whatever you pass it.'''
    return ' '.join(str(x) for x in as_iter(xs))

def default_field_value(f: Field) -> Any:
    if callable(f.default_factory):
        return f.default_factory()
    else:
        return f.default

def rescale(xs, new_total=1.0):
    '''Returns list of xs, rescaled to sum to new_total.'''
    if not xs:
        return xs
    s = sum(xs)
    if s == 0:
        x = new_total / len(xs)
        return [x] * len(xs)
    else:
        multiplier = new_total / sum(xs)
        return [multiplier * x for x in xs]

def rescale_to_max(xs: Sequence[float]) -> Iterable[float]:
    '''Returns xs, rescaled so that max(xs) == 1.0.'''
    #TODO Deal with it in a nice way if max(xs) <= 0.0.'''
    if not xs:
        return xs
    m = max(xs)
    if m <= 0.0:
        offset = 1.0 - m
        for x in xs:
            yield x + offset
    else:
        for x in xs:
            yield x / m

def reweight(xs: Sequence[float], s: float) -> Iterable[float]:
    '''See Ben's notes, 23-Dec-2019. 0.0 <= s <= 1.0. s small scales weights to
    be all nearly 1.0 except for the very lowest ones. s large scales weights
    to be be all nearly 0.0 except for the very highest ones. If s == 0.5,
    the weights will be unchanged. 's' stands for sensitivity to the weights.'''
    if not xs:
        return []
    p = 10 ** (4 * s - 2)
    m = max(xs)
    #print('REWxs', xs)
    #print('REWrxs', list(rescale_to_max(xs)))
    for x in rescale_to_max(xs):
        #print('REWx', x, x ** p)
        yield x ** p

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

def always_true(*args, **kwargs) -> bool:
    return True

def always_false(*args, **kwargs) -> bool:
    return False

def clip(lb, ub, x):
    '''Passing None for lb and/ub gives that bound no effect.'''
    if lb is not None and x <= lb:
        return lb
    elif ub is not None and x >= ub:
        return ub
    else:
        return x

def pairwise(iterable):
    iterable1, iterable2 = tee(iterable)
    next(iterable2, None)
    return zip(iterable1, iterable2)

def filter_none(f, iterable):
    xs = [f(i) for i in iterable if i is not None]
    return [x for x in xs if x is not None]

# TODO UT
def intersection(*sets: Iterable) -> Set:
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

# TODO UT
def union(*sets: Iterable) -> Set:
    return set().union(*sets)

def first(iterable):
    '''Returns first element in iterable, or None if iterable is empty.'''
    for x in as_iter(iterable):
        return x

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

#TODO Redo with contextlib.contextmanager
@dataclass
class PushAttr(AbstractContextManager):
    o: SimpleNamespace
    attr_name: str
    saved_value: Any = None

    def __enter__(self):
        self.saved_value = getattr(self.o, self.attr_name)
        return self

    def __exit__(self, *args, **kwargs):
        setattr(self.o, self.attr_name, self.saved_value)
        return None

@dataclass(frozen=True)
class Quote:
    '''For when you want to hold a value in a way that you can
    distinguished from values that you use to represent other things.'''
    value: Any

    @classmethod
    def get(cls, x):
        if isinstance(x, Quote):
            return x.value
        else:
            return x

class ClassStrIsName(type):
    def __str__(self):
        return self.__name__

# Class decorators

def singleton(cls):
    """
    By Siddhesh Suhas Sathe. Copied from:
    https://github.com/siddheshsathe/handy-decorators/blob/master/src/decorators.py
    Handy decorator for creating a singleton class
    Description:
        - Decorate your class with this decorator
        - If you happen to create another instance of the same class, it will return the previously created one
        - Supports creation of multiple instances of same class with different args/kwargs
        - Works for multiple classes
    Use:
        >>> from decorators import singleton
        >>>
        >>> @singleton
        ... class A:
        ...     def __init__(self, *args, **kwargs):
        ...         pass
        ...
        >>>
        >>> a = A(name='Siddhesh')
        >>> b = A(name='Siddhesh', lname='Sathe')
        >>> c = A(name='Siddhesh', lname='Sathe')
        >>> a is b  # has to be different
        False
        >>> b is c  # has to be same
        True
        >>>
    """
    previous_instances = {}
    @functools.wraps(cls)
    def wrapper(*args, **kwargs):
        if cls in previous_instances and previous_instances.get(cls, None).get('args') == (args, kwargs):
            return previous_instances[cls].get('instance')
        else:
            previous_instances[cls] = {
                'args': (args, kwargs),
                'instance': cls(*args, **kwargs)
            }
            return previous_instances[cls].get('instance')
    return wrapper

# Debugging

def pts(ls: Iterable, n=None):
    '''Prints ls as a table of strings. For debugging.'''
    for i, x in enumerate(as_iter(ls)):
        if n is not None and i >= n:
            break
        if is_iter(x):
            print(', '.join(str(y) for y in x))
        else:
            print(str(x))

def pl(x: Any):
    '''Prints x as a list, one line at a time.'''
    for a in as_iter(x):
        print(a)

def pr(x: Any, *args, **kwargs):
    '''Prints x as a list, one line at a time, alphabetized.'''
    if hasattr(x, 'pr'):
        x.pr(*args, **kwargs)
    else:
        pts(sorted(as_iter(x), key=str))
#        for s in sorted(str(a) for a in as_iter(x)):
#            print(s)
