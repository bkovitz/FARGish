# gloms.py -- A postamble for FARGish programs
#
# A nodeclass called Glom must be defined in the FARGish.

from Action import Action


def is_glom(g, nodeid):
    return issubclass(g.class_of(nodeid), Glom)

class GlomMerge(Action):

    def __init__(self, nodeids):
        self.nodeids = nodeids

    def go(self, g, actor):
        glomids, nodeids = g.partition_nodes(self.nodeids, is_glom)
        if not glomids:
            glomid = g.add_node(Glom)
        elif len(glomids) == 1:
            glomid = glomids[0]
        #TODO 2 or more existing gloms
        for nodeid in nodeids:
            g.remove_hops_from_port(nodeid, 'glom')
            g.add_edge(glomid, 'glommees', nodeid, 'glom')
