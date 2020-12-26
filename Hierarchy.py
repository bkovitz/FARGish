# Hierarchy.py -- Generic date structure for tracking ancestor/descendant
#                 relationships

from collections import defaultdict
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable

from util import as_iter


class Hierarchy:

    def __init__(self):
        self.ancestors = defaultdict(set)
        self.descendants = defaultdict(set)
        self.parents_of: Dict[Any, Dict[Any, None]] = defaultdict(dict)

    def declare_parent(self, p, *children):
        '''Declares that 'p' is an ancestor of each element of 'children',
        where each such element is viewed as_iter.'''
        for ch in children:
            for c in as_iter(ch):
                descs = set()  # c and all c's descendants
                descs.add(c)
                descs.update(self.descendants[c])
                ancs = set()   # p and all p's ancestors
                ancs.add(p)
                ancs.update(self.ancestors[p])
                # Make p and all p's ancestors ancestors of
                # c and all c's descendants
                for d in descs:
                    for a in ancs:
                        self.ancestors[d].add(a)
                # Make c and all c's descendants descendants of
                # p and all p's ancestors
                for a in ancs:
                    for d in descs:
                        self.descendants[a].add(d)

                self.parents_of[c][p] = None

    def isa(self, child, ancestor):
        '''Is 'child' a descendant of 'ancestor'? Returns True if
        child==ancestor.'''
        if child == ancestor:
            return True
        else:
            return ancestor in self.ancestors[child]

    # TODO UT
    def parent_and_all_descendants(self, parent) -> Iterable[Any]:
        yield parent
        for desc in self.descendants[parent]:
            yield desc

    def ascending_from(self, item) -> Iterable[Any]:
        yield item
        done = set([item])
        to_do = self.parents_of[item]
        while to_do:
            next_to_do = []
            for p in to_do:
                yield p
                done.add(p)
                next_to_do += [q for q in self.parents_of[p] if q not in done]
            to_do = next_to_do

    # TODO UT
    def __contains__(self, item):
        return item in self.ancestors
