# exc.py -- Custom exceptions for FARGish

from dataclasses import dataclass, replace
import dataclasses
from abc import ABC, abstractmethod
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable

from util import nice_object_repr, NiceRepr, Quote


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

@dataclass
class Fizzle(Exception):
    pass

@dataclass
class ActionFailure(Fizzle):
    action: 'Action'
    # TODO An 'actor' parameter, required; don't assume that action contains
    # an .actor.

    def _get_actor(self):
        return self.action.actor
    actor = property(_get_actor)

    def __str__(self):
        return f'{repr(self)}; actor={self.actor}'

    def __copy__(self):
        # HACK: Fixes mysterious TypeError in copy(NeedArg(...)).
        return replace(self)

@dataclass
class ActionFailureAc(ActionFailure):
    ac: 'Ac'
    actor_: 'MaybeNRef'

@dataclass
class ActionBlocked(Fizzle):
    action: 'Action'
    # TODO An 'actor' parameter, required; don't assume that action contains
    # an .actor.

    def _get_actor(self):
        return self.action.actor
    actor = property(_get_actor)

    def __str__(self):
        return f'{repr(self)}; actor={self.actor}'

    def __copy__(self):
        # HACK: Fixes mysterious TypeError in copy(NeedArg(...)).
        return replace(self)

@dataclass
class NeedArg(ActionBlocked):
    name: str

    def __str__(self):
        #return f"NeedArg({repr(self.action)}, {repr(self.name)}; actor={self.actor})"
        return f'NeedArg({repr(self.name)})'

@dataclass
class AcFailed(Exception):
    ac: 'Ac'
    actor: 'MaybeNRef'
    env: 'AcEnv'

    @classmethod
    def from_env(cls, **kwargs) \
    -> Callable[['G', 'Ac', 'MaybeNRef', 'AcEnv'], 'AcFailed']:
        '''Returns a function that constructs the exception, supplying
        the given keyword arguments and values. String values get looked
        up in env.'''
        def ctor(g, ac, actor, env) -> AcFailed:
            kws = {}
            for k, v in kwargs.items():
                if isinstance(v, str):
                    kws[k] = ac.get_or_none(g, actor, env, v)
                else:
                    kws[k] = Quote.get(v)
            return cls(ac, actor, env, **kws)
        return ctor

    def as_action_failure(self, action: 'Action', actor: 'MaybeNRef') \
    -> ActionFailure:
        '''Most callers should probably override this.'''
        return ActionFailureAc(action, self.ac, actor)

@dataclass
class AcBlocked(Exception, ABC):

    @abstractmethod
    def as_action_blocked(self, action: 'Action', actor: 'MaybeNRef') \
    -> ActionBlocked:
        '''Should return the appropriate ActionBlocked to represent this
        AcBlocked, with 'action' and 'actor' filled in.'''
        pass
    
@dataclass
class AcNeedArg(AcBlocked):
    ac: 'Ac'
    name: str

    def as_action_blocked(self, action, actor):
        # TODO Provide some way to store the Ac in the exception.
        return NeedArg(action=action, name=self.name)

class NoSuchNode(Exception):
    pass

@dataclass
class NoSuchNodeclass(Exception):
    name: str
    class_args: Union[List[Any], None]=None
    class_kwargs: Union[Dict[str, Any], None]=None

    def with_args(self, args, kwargs) -> 'NoSuchNodeclass':
        return dataclasses.replace(self, class_args=args, class_kwargs=kwargs)

    def __str__(self):
        return f'No such node class: {self.name}. args={self.class_args} kwargs={self.class_kwargs}'

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

@dataclass
class AcError(FargError):
    ac: 'Ac'
    exc: Exception
    env: 'AcEnv'

class TooManyArgs(FargError):
    pass

class TooManyArgs0(FargError):
    def __init__(self, node_params, args):
        self.node_params = node_params
        self.args = args

    def __str__(self):
        return f'TooManyArgs0: {self.node_params} args={self.args}'
