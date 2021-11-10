# util.py -- Generic utility functions and classes for FARGish

from collections.abc import Iterable
import collections
import random
import sys
from inspect import isclass
from dataclasses import dataclass, Field, fields, is_dataclass, InitVar, field
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, \
    Iterator, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, IO, \
    runtime_checkable, get_origin, get_args
import typing
from contextlib import AbstractContextManager
from types import SimpleNamespace
from itertools import chain, tee, filterfalse
import functools
import csv


# Useful global constants

empty_set: FrozenSet = frozenset()
newline = '\n'
backslash = '\\'

# Run-time type-checking

def is_type_instance(o, typ) -> bool:
    '''Returns true iff 'o' is an instance of the type annotation 'typ'.
    'typ' should be the result of calling typing.get_type_hints(), so it
    will be a class object if the annotation in question designates a
    class.

    Can match 'o' against any class type, but only supports a tiny subset of
    generic types: Union and Dict. As of 25-Oct-2021, this is all that is
    needed for FARGish.'''
    try:
        return isinstance(o, typ)
    except TypeError:
        if typ is None:
            return o is None
        if typ is Any:
            return True
        # if we got here, then typ must be a generic type
        origin = get_origin(typ)
        args = get_args(typ)
        if origin == Union:
            return any(is_type_instance(o, arg) for arg in args)
        #print('ORG', typ, origin, args)
        if safe_issubclass(origin, type):
            if not isclass(o):
                return False
            elif not args:  # Type without parameter matches all class objects
                return True
            else:
                return safe_issubclass(o, args[0])
        elif safe_issubclass(origin, tuple):
            if not isinstance(o, tuple):
                return False
            elif len(args) == 2 and args[1] == ...:  # if Tuple[T, ...]
                elem_type = args[0]
                return all(is_type_instance(elem, elem_type) for elem in o)
            elif len(o) != len(args):
                return False
            else:
                return all(is_type_instance(elem, arg)
                           for elem, arg in zip(o, args))
        elif safe_issubclass(origin, dict):
            if not isinstance(o, dict):
                return False
            elif o:
                k, v = first(o.items())
                ktyp, vtyp = args
                return is_type_instance(k, ktyp) and is_type_instance(v, vtyp)
            else:  # if the dictionary is empty, just say it's a match
                return True
        else:
            raise NotImplementedError(
                f"is_type_instance: Can't check {o} against {typ}."
            )

def make_nonoptional(typ: Any) -> Any:
    '''If 'typ' is a Union containing None, returns the Union without the
    None. Otherwise returns 'typ'.'''
    if get_origin(typ) == Union:
        args = get_args(typ)
        new_args = tuple(arg for arg in args if arg is not type(None))
        if args != new_args:
            return Union[new_args]
        else:
            return typ
    else:
        return typ

def safe_issubclass(o: Any, typ: Union[Type, Tuple[Type, ...]]) -> bool:
    '''Like issubclass but if o is not a class object, returns False instead
    of raising TypeError.'''
    try:
        return issubclass(o, typ)
    except TypeError:
        return False

# Conveniences for iteration and similar things

def is_iter(o):
    return (
        isinstance(o, Iterable)
        and
        not isinstance(o, str)
        and
        not is_namedtuple(o)
        and
        not isinstance(o, dict)
    )

def is_namedtuple(o):
    #HACK
    return hasattr(o, '_fields')

def as_iter(o) -> Iterable:
    '''Returns o in a form that the caller can iterate over. If o is already an
    iterable (but not a string, dictionary, or namedtuple), returns o. If o is
    none, returns an empty list. If o is anything else, returns a one-element
    list containing o.'''
    if isinstance(o, str):
        return [o]
    elif is_iter(o):
        return o
    elif o is None:
        return []
    else:
        return [o]

def as_list(o) -> List:
    if isinstance(o, list):
        return o
    else:
        return list(as_iter(o))

def as_set(o) -> Set:
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
    return tuple( # type: ignore
        (k, v) for k, v in kwargs.items()
    )

def as_dict(x: Union[Dict, None, Collection[Tuple[str, Hashable]]]) -> Dict:
    # TODO Update type annotation to show that x can be a dataclass.
    if isinstance(x, dict):
        return x
    elif x is None:
        return dict()
    elif is_dataclass_instance(x):
        # dataclasses.asdict() fails on many objects because it recursively
        # makes dictionaries for all of x's members. Here, we don't recurse.
        return dict(
            (name, getattr(x, name)) for name in field_names(x)
        )
    else:
        return dict(x)

def asdict_with_classvars(x) -> Dict[str, Any]:
    '''Does not recurse (see dataclasses._asdict_inner() for how to do that
    right), and fails if x lacks a class variable declared in x's class
    definition.'''
    return dict(
        (name, getattr(x, name))
            for name in x.__dataclass_fields__
    )

def as_name(x):
    try:
        return x.name
    except AttributeError:
        return str(x)

def is_dataclass_instance(x):
    return is_dataclass(x) and not isinstance(x, type)

def short(o) -> str:
    '''Returns a short string representation of o. If o has a .short() method
    define, we call it and return its result. Otherwise we return str(o).'''
    if isinstance(o, list):
        return f"[{', '.join(short(x) for x in o)}]"
    elif isinstance(o, tuple):
        return f"({', '.join(short(x) for x in o)})"
    elif isinstance(o, set):
        return f"{{{', '.join(short(x) for x in o)}}}"
    elif isclass(o):
        return o.__name__
    elif isinstance(o, str):
        return repr(o)
    else:
        try:
            return o.short()
        except AttributeError:
            return str(o)

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
    return False

def class_of(x: Any) -> bool:
    '''Returns x's class, or, if x is a class object, returns x.'''
    if isclass(x):
        return x
    else:
        return x.__class__
    
# TODO rm (OAOO Node.py)
def is_nodeid(x):
    return isinstance(x, int)

def write_dict_to_csv(
    self,
    d: Dict,
    mode: str,
    prefix: Optional[Sequence]=None,
    filename: str='a.csv'
) -> None:
    with open(filename, mode=mode, newline='') as csvfile:
        writer = csv.writer(csvfile, quoting=csv.QUOTE_NONNUMERIC)
        for key, value in d.items():
            writer.writerow(as_list(prefix) + [short(key), value])

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

@dataclass
class HasRngSeed:
    '''A mix-in to give a class a random-number seed. The seed is taken from
    from __init__'s args if available, or initialized randomly if not.

    A flaw due to dataclass inheritance: every class that mixes in HasRngSeed
    must have default values for all of its fields.'''
    seed: Union[int, None] = None  # must become int after .__post_init__

    def __post_init__(self):
        self.seed = reseed(self.seed)

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

def dict_str(d: Dict, xform: Callable=repr) -> str:
    '''Returns a string containing all the keys and values of d in the
    format key=repr(value) key=repr(value) etc.'''
    return ' '.join(f'{k}={xform(v)}' for k, v in d.items())

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
    if callable(f.default_factory):  # type: ignore
        return f.default_factory()  # type: ignore
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
def intersection(*iters: Iterable) -> Set:
    '''Returns a set, which is the intersection of sets. The sets may be
    any iterable, not just 'set' objects.'''
    sets = [
        s if isinstance(s, set) else set(s)
            for s in iters
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

def first_non_none(iterable):
    '''Returns the first non-None in iterable, or None if there isn't one.'''
    for x in as_iter(iterable):
        if x is not None:
            return x

# Recipe from https://docs.python.org/3.7/library/itertools.html
def unique_everseen(iterable, key=None):
    "List unique elements, preserving order. Remember all elements ever seen."
    # unique_everseen('AAAABBBCCDAABBB') --> A B C D
    # unique_everseen('ABBCcAD', str.lower) --> A B C D
    seen = set()
    seen_add = seen.add
    if key is None:
        for element in filterfalse(seen.__contains__, iterable):
            seen_add(element)
            yield element
    else:
        for element in iterable:
            k = key(element)
            if k not in seen:
                seen_add(k)
                yield element

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
    o: object  #SimpleNamespace
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

"""
trace_indent_level: int = 0

def trace(func):
    '''Function decorator: prints the name and arguments of the function each
    time it is called, and prints its return value when it returns.
    Caution: 'trace' will read generators all the way to their end.'''
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        global trace_indent_level
        argstring = ''
        if args:
            argstring += ', '.join(repr(a) for a in args)
        if kwargs:
            if argstring:
                argstring += ', '
            argstring += ', '.join(
                f'{name}={value}' for name, value in kwargs.items()
            )
        pre = ' ' * trace_indent_level
        print(f'{pre}{func.__name__}({argstring})')
        trace_indent_level += 2
        result = func(*args, **kwargs)
        trace_indent_level -= 2
        print(f'{pre}-> {result}')
        return result
    return wrapper
"""

def pts(ls: Iterable, n=None, key=str):
    '''Prints ls as a table of strings. For debugging.'''
    for i, x in enumerate(as_iter(ls)):
        if n is not None and i >= n:
            break
        if is_iter(x):
            print(', '.join(key(y) for y in x))
        else:
            print(key(x))

def pl(x: Any, key=str):
    '''Prints x as a list, one line at a time.'''
    for a in as_iter(x):
        print(key(a))

def pr(x: Any, *args, key=short, **kwargs):
    '''Prints x as a list, one line at a time, alphabetized.'''
    if hasattr(x, 'pr'):
        x.pr(*args, **kwargs)
    elif isinstance(x, dict):
        pts(sorted(x.items(), key=key), key=key)
    else:
        pts(sorted(as_iter(x), key=key), key=key)
#        for s in sorted(str(a) for a in as_iter(x)):
#            print(s)
