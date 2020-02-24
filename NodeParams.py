# NodeParam.py -- Classes that describe parameters passed to a Node when
#                 constructing it

from abc import ABC, abstractmethod

from util import as_iter


class NodeParam(ABC):
    '''An abstract specification of an argument to be passed to a Node's
    constructor.'''

    @abstractmethod
    def install_arg(self, g, thisid, datum, kwargs):
        '''Take argument value from kwargs and do whatever initialization is
        appropriate to the node thisid and datum. datum is g.datum(thisid).'''
        pass

class MateParam(NodeParam):

    def __init__(self, this_port_label, that_port_label):
        self.this_port_label = this_port_label
        self.that_port_label = that_port_label

    def install_arg(self, g, thisid, datum, kwargs):
        try:
            mateid = kwargs[self.this_port_label]
        except KeyError:
            return
        for thatid in as_iter(mateid):
            g.add_edge(
                thisid,
                self.this_port_label,
                thatid,
                self.that_port_label
            )

class AttrParam:
    
    def __init__(self, attr_name):
        self.attr_name = attr_name

    def install_arg(self, g, thisid, datum, kwargs):
        try:
            v = kwargs[self.attr_name]
        except KeyError:
            return
        setattr(datum, self.attr_name, v)

class NodeParams:

    def __init__(self, *params):
        self.params = params  # Each param should be a NodeParam

    def install_args(self, g, thisid, datum, kwargs):
        for param in self.params:
            param.install_arg(g, thisid, datum, kwargs)
