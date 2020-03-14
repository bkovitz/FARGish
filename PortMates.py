# PortMates.py -- PortMates class: records which port labels link to each other

from collections import defaultdict

from util import as_iter


class PortMates:

    def __init__(self):
        self.d = defaultdict(set)  # label: set(label)
        self.canonical = {}        # label: label

    def add(self, from_label, to_label):
        for fl in as_iter(from_label):
            for tl in as_iter(to_label):
                self._add(fl, tl)
                self._add(tl, fl)

    def _add(self, label1, label2):
        if label1 not in self.canonical:
            self.canonical[label1] = label2
        self.d[label1].add(label2)

    def auto_link(self, g, from_node, port_label, to_node):
        # TODO This should select a more appropriate port_label for to_node
        # if one is available, like a port that to_node already has.
        try:
            to_label = self.canonical[port_label]
        except KeyError:
            return  # do nothing if port_label has no mate
        g.add_edge(from_node, port_label, to_node, to_label)
