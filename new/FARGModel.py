# FARGModel.py


from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, \
    Iterator, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable

from Workspace import Workspace


@dataclass
class FARGModel(Workspace):
    '''A generic FARG model.'''

