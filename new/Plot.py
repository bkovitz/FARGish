#! /usr/bin/env python3
import csv
from csv import DictReader
from collections import defaultdict
from dataclasses import dataclass, field
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, IO, \
    runtime_checkable
from heapq import nlargest

import matplotlib.pyplot as plt  # type: ignore[import]

from FMTypes import Node
from Propagator import NodeTimeseries
from util import first, short, trace, pr, pts, pl


def readcsv(filename: str) -> List[NodeTimeseries]:
    result: Dict[Node, NodeTimeseries] = {}
    with open(filename, newline='') as csvfile:
        reader = DictReader(csvfile)
        for row in reader:
            node = row['node']
            subt = int(row['subt'])
            a = float(row['a'])
            try:
                ts = result[node]
            except KeyError:
                ts = NodeTimeseries(node)
                result[node] = ts
            ts.add(subt, a)
    return list(result.values())

def tsplot(*tss: NodeTimeseries) -> None:
    '''Put a time-series plot on the screen.'''
    plt.ion()
    plt.clf()
    plt.xlabel('t')
    plt.ylabel('a')
    for ts in tss:
        ts.plot()
    max_t = max((ts.max_t() for ts in tss), default=0)
    max_a = max((ts.max_a() for ts in tss), default=0.0)
    plt.axis([0, max_t, 0, max_a])
    plt.legend()
    

if __name__ == '__main__':
    import sys

    try:
        filename = sys.argv[1]
    except IndexError:
        filename = 'Want.csv'
    tss = readcsv(filename)
    z = nlargest(20, tss, lambda ts: ts.max_a())
    #tsplot(*tss)
    tsplot(*z)
    pr(z)
