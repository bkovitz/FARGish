# PortMates.py -- PortMates class: records which port labels link to each other

from collections import defaultdict
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar

from Node import NRef, PortLabel
from util import as_iter


class PortMates:

    def __init__(self, pairs=None):
        '''pairs is an iterable of (from_label, to_label).'''
        self.d = defaultdict(set)  # label: set(label)
        self.canonical = {}        # label: label
        if pairs:
            for f, t in pairs:
                self.add(f, t)

    def add(self, from_label, to_label):
        for fl in as_iter(from_label):
            for tl in as_iter(to_label):
                self._add(fl, tl)
                self._add(tl, fl)

    def _add(self, label1, label2):
        if label1 not in self.canonical:
            self.canonical[label1] = label2
        self.d[label1].add(label2)

    # TODO UT
    def __iadd__(self, pairs: Union['PortMates', List[Tuple[str, str]]]):
        if isinstance(pairs, PortMates):
            pairs = pairs.pairs()
        for f, t in pairs:
            self.add(f, t)
        return self
        
    # TODO UT
    def pairs(self) -> Iterable[Tuple[str, str]]:
        for f, ts in self.d.items():
            for t in ts:
                yield (f, t)
        
    def auto_link(
        self,
        g: 'ActiveGraph',
        from_node: NRef,
        port_label: PortLabel,
        to_node: NRef
    ):
        # TODO This should select a more appropriate port_label for to_node
        # if one is available, like a port that to_node already has.
        try:
            to_label = self.canonical[port_label]
        except KeyError:
            #return  # do nothing if port_label has no mate
            raise NotImplementedError
        for f in as_iter(from_node):
            for t in as_iter(to_node):
                g.add_edge(f, port_label, t, to_label)

    def is_port_label(self, name):
        return name in self.canonical

    def __repr__(self):
        #TODO Make repr string set the canonical mate for each port label
        # correctly.
        return f"PortMates({', '.join(str(p) for p in self.d.items())})"
