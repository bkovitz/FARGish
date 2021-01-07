# exc.py -- Custom exceptions for FARGish

from dataclasses import dataclass, replace, fields
import dataclasses
from abc import ABC, abstractmethod
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable

from util import nice_object_repr, NiceRepr, Quote


class FargDone(Exception):
    succeeded = False

    def done_msg(self):
        return 'FargDone' # Subclasses should override this

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

    def __str__(self):
        if not fields(self):
            return self.__class__.__name__
        else:
            return repr(self)

@dataclass
class FizzleWithTag(Fizzle, ABC):
    '''An Action was unable to run, and needs to place a tag to indicate the
    problem. Any exception that needs a Failed or Blocked tag should inherit
    from FizzleWithTag. g.do_action() catches FizzleWithTag.'''

    msg_prefix: ClassVar[str] = None  # Subclass must provide this
    tagclass_: ClassVar['CRef']

    def action_msg(self, g: 'G', actor: 'MaybeNRef') -> str:
        '''The message that should appear in the log file when an Action
        throws this exception.'''
        return f'{self.msg_prefix}  {self}'

    def place_tag(self, g: 'G', actor: 'MaybeNRef'):
        '''Should do whatever needs to be done on behalf of 'actor' when an
        Action throws this exception. Default implementation: tags 'actor'
        with a node of 'tagclass_' with a 'reason' field set to this
        exception.'''
        actor = g.as_node(actor)
        tag = g.add_node(self.tagclass_, reason=self, taggees=actor)
        g.set_activation_from_to(actor, tag)
        g.add_support(actor, tag, 1.0)
        actor.transient_inhibit_all_next()
        g.reset_activation(actor)

    @classmethod
    def from_env(cls, **kwargs):
        def ctor(g, ac, actor, env) -> FizzleAndFail:
            kws = {}
            for k, v in kwargs.items():
                if isinstance(v, str):
                    kws[k] = ac.get_or_none(g, actor, env, v)
                else:
                    kws[k] = Quote.get(v)
            return cls(**kws)
        return ctor

@dataclass
class FizzleAndBlock(FizzleWithTag):
    msg_prefix = 'blocked'
    tagclass_ = 'Blocked'
            
@dataclass
class FizzleAndFail(FizzleWithTag):
    msg_prefix = 'failed'
    tagclass_ = 'Failed'

@dataclass
class NeedArg(FizzleAndBlock):
    name: str
    ac: Union['Ac', 'Action', None] = None

    agent_nodeclass: ClassVar['CRef'] = 'FillParamScout'

    def __str__(self):
        return f'{self.__class__.__name__}({repr(self.name)})'

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
