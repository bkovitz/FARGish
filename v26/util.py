# util.py -- Generic utility functions and classes for FARGish

from __future__ import annotations
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
from types import SimpleNamespace, MethodType, GeneratorType, FunctionType
from itertools import chain, tee, filterfalse
import functools
import csv
import sys
from io import StringIO
from statistics import fmean, stdev, median


# Useful global constants

# TODO Declare these 'final'
empty_set: FrozenSet = frozenset()
newline = '\n'
backslash = '\\'

### Types

TypeAnnotation = Any  # In lieu of a type annotation for 'type annotation'
Numeric = Union[int, float]
T = TypeVar('T')
U = TypeVar('U')

### as_ utilities: for coercing objects to a given type

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

def as_tuple(o) -> Tuple:
    if isinstance(o, tuple):
        return o
    elif hasattr(o, 'as_tuple'):
        return o.as_tuple()  # type: ignore[arg-type]
    else:
        return tuple(as_iter(o))

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

def as_name(x):
    try:
        return x.name
    except AttributeError:
        return str(x)

### Simple predicates

def is_iter(o: Any) -> bool:
    '''Is o iterable for for purposes of as_iter(), i.e. iterable but not a
    string, namedtuple, or dict?'''
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

def always_true(*args, **kwargs) -> bool:
    return True

def always_false(*args, **kwargs) -> bool:
    return False

# TODO rm (OAOO Node.py)
def is_nodeid(x):
    return isinstance(x, int)

### Utilities for working with iterables

def filter_none(f, iterable):
    xs = [f(i) for i in iterable if i is not None]
    return [x for x in xs if x is not None]

def first(iterable):
    '''Returns first element in iterable, or None if iterable is empty.'''
    for x in as_iter(iterable):
        return x

def first_non_none(iterable):
    '''Returns the first non-None in iterable, or None if there isn't one.'''
    for x in as_iter(iterable):
        if x is not None:
            return x

def pairwise(iterable):
    iterable1, iterable2 = tee(iterable)
    next(iterable2, None)
    return zip(iterable1, iterable2)

def vcat(a, b):
    '''Concatenate value(s). Combines a and b into either a list or a
    single value, preferring the latter. If a is an iterable, modifies a
    and returns it. But if a is None or a non-iterable, vcat creates a
    new list and returns that. So, calling code should look like this:
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

# TODO UT
def transitive_closure(more_xs: Callable[[T], Iterable[T]], x: T) -> Set[T]:
    result: Set[T] = {x}
    pending: Set[T] = {x}
    new_xs: Set[T] = set()
    while pending:
        for old_x in pending:
            for new_x in more_xs(old_x):
                if new_x not in result:
                    new_xs.add(new_x)
        result |= new_xs
        pending = new_xs
        new_xs.clear()
    return result

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

### Utilities for working with dictionaries

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

#TODO UT
def loose_dict_eq(d1: Dict, d2: Dict) -> bool:
    '''Are d1 and d2 equal, if we count a value of None as equal to not having
    a value at all?'''
    for k in set(chain(d1.keys(), d2.keys())):
        if d1.get(k, None) != d2.get(k, None):
            return False
    return True

def dupdate(d: Dict[T, U], *args, **kwargs) -> Dict[T, U]:
    result = d.copy()
    result.update(*args, **kwargs)
    return result

### Utilities for working with tuples

# TODO Rename this to something clearer
def tupdict(**kwargs) -> Tuple[Tuple[str, Hashable]]:
    return tuple( # type: ignore
        (k, v) for k, v in kwargs.items()
    )

### Utilities for working with sets

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

### Utilities for working with dataclasses

def is_dataclass_instance(x):
    return is_dataclass(x) and not isinstance(x, type)

def force_setattr(obj, attrname, value):
    '''Writes value on obj.attrname even if obj is immutable.'''
    object.__setattr__(obj, attrname, value)

def field_names(dclass) -> List[str]:
    return [f.name for f in fields(dclass)]

def fields_for(dclass, kwargs: Dict[str, Any]) -> Dict[str, Any]:
    #print('FFOR', dclass.__name__, field_names(dclass), kwargs)
    return d_subset(kwargs, field_names(dclass))

def as_dict(x: Union[Dict, Any, None, Collection[Tuple[str, Hashable]]]) \
-> Dict:
    # TODO Update type annotation to show that x can be a dataclass.
    if isinstance(x, dict):
        return x
    elif x is None:
        return dict()
    elif is_dataclass(x):  #is_dataclass_instance(x):
        # dataclasses.asdict() fails on many objects because it recursively
        # makes dictionaries for all of x's members. Here, we don't recurse.
        return dict(
            #(name, getattr(x, name)) for name in field_names(x)
            field_names_and_values(x)
        )
    else:
        return dict(x)

def field_names_and_values(dclass) -> Iterable[Tuple[str, Any]]:
    for name in field_names(dclass):
        try:
            yield (name, getattr(dclass, name))
        except AttributeError:
            continue

# TODO UT
def instantiate_dataclass_from_kwargs(dclass: Type[T], kwargs: Dict[str, Any]) \
-> T:
    '''dclass must be a dataclass.'''
    # TODO
    #print('IDFK1', fields_for(dclass, kwargs))
    #print('IDFK2', as_dict(dclass))
    return dclass(  # type: ignore[call-arg]
        **(as_dict(dclass) | fields_for(dclass, kwargs))
    )

def as_dstr(x: Union[Dict, Any, None, Collection[Tuple[str, Hashable]]]) \
-> str:
    '''Convenient string to represent an object, especially a dataclass
    instance, showing the class name and all of the object's fields.''' 
    if x is None:
        return 'None'
    else:
        cl = x.__class__.__name__
        args = ', '.join(f'{k}={short(v)}' for k, v in as_dict(x).items())
        return f'{cl}({args})'

def asdict_with_classvars(x) -> Dict[str, Any]:
    '''Does not recurse (see dataclasses._asdict_inner() for how to do that
    right), and fails if x lacks a class variable declared in x's class
    definition.'''
    return dict(
        (name, getattr(x, name))
            for name in x.__dataclass_fields__
    )

def default_field_value(f: Field) -> Any:
    if callable(f.default_factory):  # type: ignore
        return f.default_factory()  # type: ignore
    else:
        return f.default

# TODO UT
def dc_type_of(cls: Type, field_name: str) -> Type:
    '''Returns the type of 'field_name' as defined in 'cls'. 'cls' must be
    a dataclass.'''
    for f in fields(cls):
        if f.name == field_name:
            return f.type
    raise AttributeError(f'{cls} contains no field named {field_name}.')

### Utilities for inspecting classes (including whatever class an object is)

def class_of(x: Any) -> Type:
    '''Returns x's class, or, if x is a class object, returns x.'''
    if isclass(x):
        return x
    else:
        return x.__class__
    
def safe_issubclass(o: Any, typ: Union[Type, Tuple[Type, ...]]) -> bool:
    '''Like issubclass but if o is not a class object, returns False instead
    of raising TypeError.'''
    try:
        return issubclass(o, typ)
    except TypeError:
        return False

def is_seq_of(x: Any, clas: Type) -> bool:
    if isinstance(x, list) or isinstance(x, tuple):
        try:
            return isinstance(x[0], clas)
        except IndexError:
            return False
    return False

### Utilities for working with type hints at run-time

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
        elif origin == Optional:
            return (o is None) or any(is_type_instance(o, arg) for arg in args)
        #print('ORG', type(type), typ, origin, args)
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

### Random-number and random-choice utilities

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
    number of items.

    If k is None, returns all items.'''
    if k is None:
        yield from items
    else:
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

### Descriptive statistics

@dataclass(frozen=True)
class DescStats:
    mean: Numeric
    stdev: Numeric
    n: int
    median: Numeric
    min: Numeric
    max: Numeric

    @classmethod
    def from_data(cls, data: Iterable[Numeric]) -> DescStats:
        data: List[Numeric] = as_list(data)
        return DescStats(
            fmean(data),
            stdev(data),
            len(data),
            median(data),
            min(data),
            max(data)
        )

    def __str__(self) -> str:
        return f'mean={nf(self.mean)} stdev={nf(self.stdev)} n={self.n} median={nf(self.median)} min={nf(self.min)} max={nf(self.max)}'

def nf(x: Numeric) -> str:
    '''Convenient format for floating-point numbers.'''
    return f'{x:0.3f}'

### Comma-separated values (CSVs)

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

### Utilities for making nice strings to describe objects

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
    format key=repr(value) key=repr(value) etc., in sorted order.'''
    return ' '.join(f'{k}={xform(v)}' for k, v in sorted(d.items()))

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

### Utilities for scaling and clipping

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

def clip(lb: Numeric, ub: Numeric, x: Numeric) -> Numeric:
    '''Passing None for lb and/ub gives that bound no effect.'''
    if lb is not None and x <= lb:
        return lb
    elif ub is not None and x >= ub:
        return ub
    else:
        return x

### Utilities for file I/O

def read_to_blank_line(f):
    result = ''
    while True:
        l = f.readline()
        if not l.strip():
            break
        result += l
    return result

### Class decorators

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

### Context managers

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

### Uncategorized utility functions, classes, and mix-ins

def identity(x):
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

### Debugging and reporting

def short(o) -> str:
    '''Returns a short string representation of o. If o has a .short() method
    defined, we call it and return its result. Otherwise we return str(o).'''
    if isinstance(o, list):
        return f"[{', '.join(short(x) for x in o)}]"
    elif isinstance(o, tuple):
        return f"({', '.join(short(x) for x in o)})"
    elif isinstance(o, set):
        return f"{{{', '.join(short(x) for x in o)}}}"
    elif isinstance(o, dict):
        items = [f'{repr(k)}: {short(v)}' for k, v in o.items()]
        return f"{{{', '.join(items)}}}"
    elif isinstance(o, float):
        return f'{o:1.3}'
    elif isclass(o):
        return o.__name__
#    elif isinstance(o, str):
#        return repr(o)
    elif isinstance(o, MethodType):
        return f'{short(o.__self__)}.{o.__name__}()'
    elif isinstance(o, FunctionType):
        return o.__name__
    else:
        try:
            return o.short()
        except AttributeError:
            return str(o)

def csep(xs) -> str:
    '''Comma-separated string for whatever you pass it.'''
    return ', '.join(str(x) for x in as_iter(xs))

def ssep(xs) -> str:
    '''Space-separated string for whatever you pass it.'''
    return ' '.join(str(x) for x in as_iter(xs))

def sstr(x: Any) -> str:
    if isinstance(x, tuple):
        return f"({', '.join(sstr(xx) for xx in x)})"
    elif isinstance(x, list):
        return f"[{', '.join(sstr(xx) for xx in x)}]"
    elif isinstance(x, set):
        return f"{{{', '.join(sstr(xx) for xx in x)}}}"
    else:
        return str(x)

def pts(ls: Iterable, n=None, key=sstr, file=sys.stdout):
    '''Prints ls as a table of strings. For debugging.'''
    if isinstance(ls, dict):
        pts((k, v) for k, v in ls.items())
    else:
        for i, x in enumerate(as_iter(ls)):
            if n is not None and i >= n:
                break
            if is_iter(x):
                print(', '.join(key(y) for y in x), file=file)
            else:
                print(key(x), file=file)

def pl(x: Any, key=str):
    '''Prints x as a list, one line at a time.'''
    for a in as_iter(x):
        print(key(a))

def pr(x: Any, *args, key=short, file=sys.stdout, **kwargs):
    '''Prints x as a list, one line at a time, alphabetized.'''
    if hasattr(x, 'pr'):
        x.pr(*args, key=key, file=file, **kwargs)
    elif isinstance(x, dict):
        #pts(sorted(x.items(), key=key), key=key, file=file)
        for k, v in sorted(x.items(), key=str):
            if isinstance(v, float):
                v = f'{v:1.5f}'
            print(f'{short(k)}: {short(v)}', file=file)
    else:
        pts(sorted(as_iter(x), key=key), key=key, file=file)
#        for s in sorted(str(a) for a in as_iter(x)):
#            print(s, file=file)

def prs(*args, **kwargs) -> str:
    sio = StringIO()
    pr(*args, file=sio, **kwargs)
    return sio.getvalue().rstrip()

def ps(*items: Any, file=sys.stdout) -> None:
    '''Short for print(short(i)).'''
    for i in items:
        if isinstance(i, GeneratorType):
            ps(*i, file=file)
        else:
            print(short(i), file=file)

def pss(*items: Any) -> str:
    sio = StringIO()
    ps(*items, file=sio)
    return sio.getvalue().rstrip()

def psa(*items: Any, file=sys.stdout) -> None:
    '''Alphabetized ps().'''
    for line in sorted(pss(*items).splitlines()):
        print(line, file=file)
