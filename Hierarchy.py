# Hierarchy.py -- Generic date structure for tracking ancestor/descendant
#                 relationships

from collections import defaultdict

from util import as_iter


class Hierarchy:

    def __init__(self):
        self.ancestors = defaultdict(set)
        self.descendants = defaultdict(set)

    def parent(self, p, *children):
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

    def isa(self, child, ancestor):
        '''Is 'child' a descendant of 'ancestor'? Returns True if
        child==ancestor.'''
        if child == ancestor:
            return True
        else:
            return ancestor in self.ancestors[child]
