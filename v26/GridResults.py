# GridResults.py -- Reproducible experiments on Grid.py

from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from copy import deepcopy

from Grid import Canvas, two_cs, make_ppainters
from util import DescStats, Numeric


def blank_and_regen() -> Numeric:
    '''Blanks some random cells in two_cs, runs .regenerate, and returns the
    number of cells that are still wrong.'''
    c = Canvas.from_data(two_cs)
    orig = deepcopy(c)
    pps = set(make_ppainters(c))
    lpps = list(pps)
    c.blank_random()
    print(c)
    print()
    c.regenerate(pps, niters=100)
    print(c)
    print()
    print(c.claritystr())
    print()
    result = c.levdist(orig)
    print(result)
    return result

def result_blank_and_regen(nsamples: int=30):
    data = [blank_and_regen() for _ in range(nsamples)]
    print(str(DescStats.from_data(data)))

if __name__ == '__main__':
    result_blank_and_regen()
