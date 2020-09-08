# exc.py -- Custom exceptions for FARGish

from dataclasses import dataclass
from typing import List, Dict

from util import nice_object_repr, NiceRepr


class FargDone(Exception):
    succeeded = False

    def done_msg(self):
        return 'FargDone' # This should be overridden

    __repr__ = nice_object_repr

class NumboSuccess(FargDone):
    succeeded = True

    def __init__(self, g, target):
        'g must be a NumboGraph.'
        self.g = g
        self.target = target

    def __str__(self):
        return 'Success!  ' + str(self.g.expr_as_equation(self.target))

class TooManyTimestepsWithNoResponse(FargDone):

    def __init__(self, num_timesteps):
        self.num_timesteps = num_timesteps

    def __str__(self):
        return "Giving up. %d timesteps without seeing an action." % (
            self.num_timesteps
        )

class FargCantRespond(Exception):
    pass

class Fizzle(Exception):
    pass

class NeedArg(Fizzle):
    def __init__(self, action, name):
        self.action = action
        self.name = name
    def _get_actor(self):
        return self.action.actor
    actor = property(_get_actor)
    def __str__(self):
        return f"NeedArg({repr(self.action)}, {repr(self.name)})"

@dataclass
class NodeLacksMethod(Fizzle):
    nodeid: int
    method_name: str
    method_args: List
    method_kwargs: Dict


class GeneratorDone(Fizzle):
    pass

class FARGishCompilerException(Exception):
    pass

class NoUniqueMateError(FARGishCompilerException):
    pass

class FargError(Exception, NiceRepr):
    pass

class TooManyArgs(FargError):
    pass

class TooManyArgs0(FargError):
    def __init__(self, node_params, args):
        self.node_params = node_params
        self.args = args

    def __str__(self):
        return f'TooManyArgs0: {self.node_params} args={self.args}'
