# NodeParam.py -- Classes that describe parameters passed to a Node when
#                 constructing it

from abc import ABC, abstractmethod
from collections.abc import Iterable

from exc import TooManyArgs0
from util import as_iter


def add_kwarg(name, arg, kwargs):
    try:
        v = kwargs[name]
    except KeyError:
        kwargs[name] = arg
        return
    if isinstance(v, list):
        v.append(arg)
    elif isinstance(v, tuple):
        kwargs[name] = list(v) + [arg]
    else:
        kwargs[name] = [v, arg]

class NodeParam(ABC):
    '''An abstract specification of an argument to be passed to a Node's
    constructor.'''

    @abstractmethod
    def install_arg(self, g, thisid, datum, kwargs):
        '''Take argument value from kwargs and do whatever initialization is
        appropriate to the node thisid and datum. datum is g.datum(thisid).'''
        pass

    @abstractmethod
    def arg_into_kwargs(self, arg, kwargs):
        '''Add arg into kwargs as a named argument.'''
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

    def arg_into_kwargs(self, arg, kwargs):
        add_kwarg(self.this_port_label, arg, kwargs)

class AttrParam:
    
    def __init__(self, attr_name):
        self.attr_name = attr_name

    def install_arg(self, g, thisid, datum, kwargs):
        try:
            v = kwargs[self.attr_name]
        except KeyError:
            return
        setattr(datum, self.attr_name, v)

    def arg_into_kwargs(self, arg, kwargs):
        add_kwarg(self.attr_name, arg, kwargs)

class NodeParams:

    def __init__(self, *params):
        self.params = params  # Each param should be a NodeParam

    def install_args(self, g, thisid, datum, kwargs):
        for param in self.params:
            param.install_arg(g, thisid, datum, kwargs)

    def args_into_kwargs(self, args, kwargs):
        '''Converts args into named arguments in kwargs. Modifies kwargs.'''
        if len(args) > len(self.params):
            raise TooManyArgs0(args)
        for arg, param in zip(args, self.params):
            param.arg_into_kwargs(arg, kwargs)

    def __len__(self):
        return len(self.params)
