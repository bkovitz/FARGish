# FMTypes.py -- Fundamental 'typing' Type definitions for FARGModel and
# related classes

from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence


# An element of the workspace or a node in the slipnet
Elem = NewType('Elem', Hashable)

# A value that can be stored in a Canvas cell
Value = NewType('Value', Hashable)

# The address of a Canvas cell within its Canvas
Addr = NewType('Addr', Hashable)
