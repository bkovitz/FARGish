# Ac.py -- Subactions

from abc import ABC, abstractmethod
from dataclasses import dataclass, field, fields, replace
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable
from contextlib import contextmanager
import math

from Node import Node, NRef, NRefs, MaybeNRef, PortLabel, PortLabels, MaybeCRef
from NodeParams import NodeParams, AttrParam, MateParam
from Action import Action, Actions
from ActiveNode import ActionNode, Start, Completed, ActiveNodeState
from criteria import Criterion, Criteria, OfClass
from exc import Fizzle, NeedArg, AcError, FargDone, FizzleAndFail, \
    FizzleAndBlock
from StdGraph import Context, MyContext, pg
from util import as_set, Quote, as_iter, as_list, is_seq_of, always_true, \
    first, clip

AcEnv = dict
'''A binding environment for execution of an Ac.'''

@dataclass
class AcFizzle(Fizzle):
    '''Exception indicating that an Acs should fail silently.'''
    pass

@dataclass
class AcFalse(AcFizzle):
    '''Exception indicating that an Acs tested for some condition and it 
    was false, or searched for something and didn't find it.'''
    ac: 'Acs'
    actor: NRef
    env: AcEnv

@dataclass
class CantFind(FizzleAndFail):
    '''We looked for a node meeting certain criteria and didn't find one.'''
    criteria: Criteria

@dataclass
class NotEqualValue(FizzleAndFail):
    node1: MaybeNRef
    node2: MaybeNRef

@dataclass
class Ac(ABC):
    name_overrides_: ClassVar[Dict[str, str]] = {} #field(init=False, default_factory=lambda: {})

    def get(
        self,
        g: 'G',
        actor: MaybeNRef,
        env: AcEnv,
        name: str,
        default: Any=None
    ) -> Any:
        '''Looks up name, searching first in actor's overrides, then in env,
        then in actor's attrs, and finally in this Ac's attrs. If the
        value found is itself a string, then looks up the string as 'name'
        (recursively). A value of Quote(s) will be returned as s, without
        recursive lookup, if you need to actually return a string.
        Raises NeedArg if the value found is None or not found.'''
        # TODO Document name_overrides_
        # TODO Document special meaning of 'this'
        if name == 'this':
            return actor
        #print('ACNAME0', name, self.name_overrides_)
        name = self.name_overrides_.get(name, name)
        #print('ACNAME1', name)
        try:
            result = g.get_overrides(actor, name)[name]
        except KeyError:
            try:
                result = env[name]
                #print('ACGETENV', self.__class__.__name__, name, repr(result))
            except KeyError:
                result = g.getattr(actor, name)
                if result is None:
                    try:
                        #print('GET', self, repr(name))
                        result = getattr(self, name)
                        #print('GET1', result)
                    except AttributeError:
                        result = default

        #print('ACGET2', self.__class__.__name__, name, repr(result))
        if result is None:
            #raise AcNeedArg(ac=self, name=name)
            raise NeedArg(ac=self, name=name)
        elif isinstance(result, str):
            # TODO Prevent infinite recursion: make sure we haven't tried
            # this key before.
            #print('GETRECUR', self, actor, name, result)
            result = self.get(g, actor, env, result)
            #print('GETRECUR2', repr(result))
            return result
        elif Context.is_context(result):
            result = result.focal_point(g, actor)
            #print('GETCTX', result)
            return result
        elif is_seq_of(result, Criterion):
            #return [self.fix_criterion(g, c, actor, env) for c in result]
            return [c.replace_from_env(g, self, actor, env) for c in result]
        elif isinstance(result, Criterion):
            return result.replace_from_env(g, self, actor, env)
        else:
            return Quote.get(result)

    # TODO UT
    def get_or_fizzle(self, g: 'G', actor: MaybeNRef, env: AcEnv, name: str) \
    -> Any:
        '''Same as .get() but raises AcFizzle if can't find value for 'name'.'''
        try:
            return self.get(g, actor, env, name)
        except NeedArg:
            raise AcFizzle

    # TODO UT
    def get_or_none(self, g: 'G', actor: MaybeNRef, env: AcEnv, name: str) \
    -> Any:
        '''Same as .get() but returns None if can't find value for 'name'.'''
        try:
            return self.get(g, actor, env, name)
        except NeedArg:
            return None

    @contextmanager
    def push_name_overrides(self, d: Dict[str, str]):
        old = self.name_overrides_
        new = old.copy()
        new.update(d)
        try:
            self.name_overrides_ = new
            yield self
        finally:
            self.name_overrides_ = old
            
    @abstractmethod
    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        '''Do the partial Action defined by this Ac. Raise an ActionBlocked
        exception if missing an argument.'''
        pass

    @classmethod
    def run(
        cls,
        g: 'ActiveGraph',
        acs: Union['Ac', Iterable['Ac'], None],
        actor: NRef
    ) -> AcEnv:
        '''Runs acs, starting from an empty AcEnv. If any Ac throws AcFalse,
        run() catches it and returns the env from the AcFalse exception.
        This makes run() suitable for running acs as an Action, but not as a
        subroutine called from inside an Ac. For that, see Ac.call().'''
        env = AcEnv()
        #try:
        cls.call(g, acs, actor, env)
#        except AcFalse as exc:
#            print('RUN', exc.__class__, exc)
#            return exc.env
#        except AcFizzle:
#            pass
        return env

    @classmethod
    def call(
        cls,
        g: 'ActiveGraph',
        acs: Union['Ac', Iterable['Ac'], None],
        actor: NRef,
        env: AcEnv
    ) -> None:
        '''Runs acs, starting from given AcEnv. env will likely be modified.'''
        for ac in as_iter(acs):
            try:
                ac.go(g, actor, env)
            except (AcFalse, FargDone, Fizzle):
                raise
            except Exception as exc:
                raise AcError(ac, exc, env)

    @classmethod
    def as_action(cls, acs: Union['Ac', Sequence['Ac'], None], **kwargs) \
    -> Action:
        return AcAction(acs, **kwargs)

Acs = Union[Ac, Iterable[Ac], None]

#TODO UT that overrides the value for the overridden name
@dataclass
class WithNameOverride(Ac):
    ac: Ac
    name_overrides: Dict[str, str]

    def __init__(self, ac: Ac, **kwargs):
        self.ac = ac
        self.name_overrides = kwargs
        for oldname, newname in kwargs.items():
            setattr(self.ac, newname, getattr(self.ac, oldname))
            # TODO delattr the old name?

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        # TODO Refactor so we don't need to modify self.ac. That's bad
        # design because ac could be shared among multiple AcNodes.
        with self.ac.push_name_overrides(self.name_overrides):
            #print('WNAME', self.name_overrides)
            self.ac.go(g, actor, env)

@dataclass
class AcAction(Action):
    acs: Union[Ac, Sequence[Ac], None]

    def __init__(self, acs, threshold: float=0.0, **kwargs):
        super().__init__(**kwargs)
        self.acs = acs
        self.threshold = threshold

    def go(self, g, actor):
        Ac.run(g, self.acs, actor)
#        try:
#            Ac.run(g, self.acs, actor)
#        except AcBlocked as exc:
#            raise exc.as_action_blocked(self, actor)
#        except AcFailed as exc:
#            raise exc.as_action_failure(self, actor)
#        except AcFizzle:
#            return

@dataclass
class All(Ac):
    criterion: Acs = None
    focal_point: MaybeNRef = None   #TODO Allow MyContext

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        criterion = self.get(g, actor, env, 'criterion')
        focal_point = self.get(g, actor, env, 'focal_point')
        env['nodes'] = g.find_all(criterion, focal_point=focal_point)

@dataclass
class LookFor(Ac):
    criterion: Criterion = None
    focal_point: MaybeNRef = None
    cond: Acs = None  # Acs to check further criteria
    asgn_to: str = 'node'

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        criterion = self.get(g, actor, env, 'criterion')
        focal_point = self.get(g, actor, env, 'focal_point')
        node = None
        #print('LNODE0', node, criterion, focal_point)
        # TODO Call g.look_for() and pass a function that checks self.cond
        for node in g.find_all(criterion, focal_point=focal_point):
            #print('LNODE', node, criterion, focal_point)
            try:
                env['node'] = node  # TODO Push/pop 'node'
                Ac.call(g, self.cond, actor, env)
                break
            except AcFalse:
                continue
        else: # TODO UT that exercises this 'else'
            node = None
        env['node'] = node
        if not node:
            raise AcFalse(self, actor, env)
        else:
            env[self.asgn_to] = node

@dataclass
class Boost(Ac):
    nodes: NRefs = None

    def go(self, g, actor, env):
        nodes = self.get(g, actor, env, 'nodes')
        a = g.activation(actor)
        if a is None:
            a = 1.0
        #boost_amount = max(a * 10.0, 0.2)
        boost_amount = clip(0.2, 10.0, a * 10.0)
        for node in as_iter(nodes):
            #print('BOOST', node)
            # TODO Make the boost_amount a function of actor's activation
            g.boost_activation(node, boost_amount)

# TODO UT
@dataclass
class AsgnNeighbors(Ac):
    node: NRefs = None
    port_label: PortLabels = None

    def go(self, g, actor, env):
        node = self.get(g, actor, env, 'node')
        port_label = self.get(g, actor, env, 'port_label')
        env[port_label] = g.neighbors(node, port_label)

# TODO UT
@dataclass
class AsgnProposedNeighbors(Ac):
    node: NRef = None
    port_label: PortLabels = None

    def go(self, g, actor, env):
        node = self.get(g, actor, env, 'node')
        port_label = self.get(g, actor, env, 'port_label')
        env['proposed'] = g.prepend_port_label_prefix(
            'proposed_',
            g.incidence_outgoing(node, port_label)
        )

# TODO UT
@dataclass
class ValueDifference(Ac):
    asgn_to: str = 'value'
    arg1: MaybeNRef = None
    arg2: MaybeNRef = None

    def go(self, g, actor, env):
        arg1 = self.get(g, actor, env, 'arg1')
        arg2 = self.get(g, actor, env, 'arg2')
        try:
            env[self.asgn_to] = g.value_of(arg1) - g.value_of(arg2)
        except TypeError:
            env[self.asgn_to] = None

#@dataclass
#class Asgn(Ac):
#    asgn_to: str = 'value'
#    acexpr: AcExpr = None
#
#    def go(self, g, actor, env):
#        acexpr = self.get(g, actor, env, 'acexpr')
#        env[self.asgn_to] = acexpr.eval(g, actor, env)

@dataclass
class LookForTup(Ac):
    criterion: Criterion = None
    focal_point: MaybeNRef = None
    tupcond: Any = always_true  # TODO appropriate type hint
    asgn_to: str = 'nodes'

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        criterion = self.get(g, actor, env, 'criterion')
        focal_point = self.get(g, actor, env, 'focal_point')
        tupcond = self.get(g, actor, env, 'tupcond')
        tup = g.look_for(criterion, focal_point=focal_point, tupcond=tupcond)
        print(f'LOOKFORTUP tup={tup}, criterion={criterion}, focal_point={focal_point}, tupcond={tupcond}')
        if not tup:
            raise AcFalse(self, actor, env)
        env[self.asgn_to] = tup

@dataclass
class AllAre(Ac):
    criterion: Union[Criterion, None] = None

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        criterion = self.get(g, actor, env, 'criterion')
        # TODO In .get(), override parameters of a Criterion.
        nodes = self.get(g, actor, env, 'nodes')
        for node in as_iter(nodes):
            if not criterion(g, node):
                raise AcFalse(self, actor, env)

@dataclass
class AcNot(Ac):
    acs: Acs

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        try:
            Ac.call(g, self.acs, actor, env)
        except AcFalse:
            pass
        else:
            raise AcFalse(self.acs, actor, env)

@dataclass
class EqualValue(Ac):
    node1: MaybeNRef = None
    node2: MaybeNRef = None

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        node1 = self.get(g, actor, env, 'node1')
        node2 = self.get(g, actor, env, 'node2')
        if g.value_of(node1) != g.value_of(node2):
            raise AcFalse(self, actor, env)

@dataclass
class MembersOf(Ac):
    focal_point: MaybeNRef = None

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        focal_point = self.get(g, actor, env, 'focal_point')
        env['nodes'] = g.members_of(focal_point)

@dataclass
class Len(Ac):
    nodes: NRefs = None
    
    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        nodes = self.get(g, actor, env, 'nodes')
        env['value'] = len(nodes)

@dataclass
class HasKwargs(Ac):  # Mix-in
    kwargs: Dict[str, Any] = None

    def __init__(self, *args, **kwargs):
        for k, v in kwargs.items():
            setattr(self, k, v)
        self.kwargs = kwargs
        
    def get_kwargs(self, g: 'G', actor: NRef, env: AcEnv) -> Dict[str, Any]:
        kwargs = {}
        for k, v in self.kwargs.items():
            if isinstance(v, str):  # TODO What if the value is properly a str?
                # If value is a string, we look it up (indirection)
                kwargs[k] = self.get(g, actor, env, v)
            else:
                kwargs[k] = v
        return kwargs
    
@dataclass
class TagWith(HasKwargs, Ac):
    tagclass: MaybeCRef = None
    taggees: NRefs = None

    def __init__(self, tagclass=None, taggees=None, **kwargs):
        self.tagclass = tagclass
        self.taggees = taggees
        super().__init__(self, **kwargs)

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        taggees = self.get(g, actor, env, 'taggees')
        tagclass = self.get(g, actor, env, 'tagclass')
        kwargs = self.get_kwargs(g, actor, env)
        tag = g.add_node(tagclass, taggees=taggees, **kwargs)
        env['result'] = tag

@dataclass
class Taggees(Ac):
    names: List[str]

    def __init__(self, *args):
        self.names = args

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        taggees = set()
        for name in self.names:
            v = self.get(g, actor, env, name)
            taggees |= as_set(v)
        env['taggees'] = taggees

@dataclass
class AddNode(HasKwargs, Ac):
    nodeclass: MaybeCRef = None

    def __init__(self, nodeclass, **kwargs):
        self.nodeclass = nodeclass
        super().__init__(self, **kwargs)

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        nodeclass = self.get(g, actor, env, 'nodeclass')
        kwargs = self.get_kwargs(g, actor, env)
        neighbors = kwargs.pop('neighbors', {})
        kwargs.update(neighbors)
#        for port_label, n in neighbors.get_items():
#            kwargs[port_label] = n
        env['node'] = g.add_node(nodeclass, **kwargs)

@dataclass
class LogValue(Ac):

    def go(self, g, actor, env):
        nodes = self.get(g, actor, env, 'nodes')
        node = first(as_iter(nodes))
        env['value'] = math.log10(g.value_of(node))

@dataclass
class DeTup(Ac):
    asgn_to: Union[str, Sequence[str]] = 'node'

    def go(self, g, actor, env):
        nodes = self.get(g, actor, env, 'nodes')
        for i, name in enumerate(self.asgn_to):
            try:
                value = nodes[i]
            except KeyError:
                value = None
            env[name] = value

@dataclass
class FindParamName(Ac):

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        problem = self.get(g, actor, env, 'problem')
        # TODO violates Law of Demeter (for Nodes)
        reason = g.getattr(problem, 'reason')
        env['name'] = Quote(reason.name)

@dataclass
class LookForArg(Ac):
    focal_point: MaybeNRef = MyContext  # TODO type hint should allow MyContext

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        focal_point = self.get(g, actor, env, 'focal_point')
        #name = self.get(g, actor, env, 'name')
        # TODO Determine the class from 'name'
        criterion = OfClass('Glom')  # HACK
        node = g.look_for(criterion, focal_point=focal_point)  # HACK
        if not node:
            #print('LOOKCANT', criterion, node, focal_point)
            raise CantFind(criterion)
        env['node'] = node

@dataclass
class AddOverride(Ac):

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        behalf_of = self.get(g, actor, env, 'behalf_of')
        node = self.get(g, actor, env, 'node')
        name = self.get(g, actor, env, 'name')
        g.add_override_node(behalf_of, name, node)
    
# TODO UT
@dataclass
class RemoveBlockedTag(Ac):

    def go(self, g, actor, env):
        problem = self.get_or_fizzle(g, actor, env, 'problem')
        g.boost_activation_from_to(actor, g.neighbors(problem, 'taggees'), 5.0)
        g.remove_node(problem)
        
@dataclass
class OrFail(Ac):
    ac: Ac
    exc: Type[FizzleAndFail]
    #argnames: Sequence[str]

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        try:
            self.ac.go(g, actor, env)
        except AcFalse:
            raise self.exc(g, self.ac, actor, env)

# TODO UT
@dataclass
class OrBlock(Ac):
    ac: Ac
    exc: Type[FizzleAndBlock]

    # TODO OAOO OrFail
    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        try:
            self.ac.go(g, actor, env)
        except AcFalse:
            raise self.exc(g, self.ac, actor, env)

@dataclass
class Calm(Ac):
    '''Greatly reduce actor's activation.'''
    
    def go(self, g, actor, env):
        g.calm(actor)

# TODO @dataclass ?
class Sleep(Ac):
    sleep_duration: int = 3  # Number of timesteps to sleep

    def go(self, g, actor, env):
        sleep_duration = self.get(g, actor, env, 'sleep_duration')
        g.sleep(actor, sleep_duration)

class SleepUntilAwakened(Ac):

    def go(self, g, actor, env):
        g.sleep(actor, None)

@dataclass
class Raise(HasKwargs, Ac):
    exc: Type[Exception] = None  # unconditionally filled in by __init__

    def __init__(self, exc, **kwargs):
        self.exc = exc
        super().__init__(self, **kwargs)

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        kwargs = self.get_kwargs(g, actor, env)
        raise self.exc(**kwargs)

@dataclass
class SelfDestruct(Ac):

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        g.remove_node(actor)

@dataclass
class NewState(Ac):
    # BUG-PRONE: naming this member 'state' would cause a name-clash with
    # ActiveNode.state.
    new_state: ActiveNodeState = Completed

    def go(self, g, actor, env):
        actor = g.as_node(actor)
        new_state = self.get(g, actor, env, 'new_state')
        actor.state = new_state

@dataclass
class PrintEnv(Ac):
    '''For debugging.'''

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        print('PRINTENV', env)

@dataclass
class PrintInfo(Ac):
    '''For debugging.'''

    def go(self, g, actor, env):
        print('INFO:', g.nodestr(actor), env)

class AcNode(ActionNode):
    '''A node that holds one or more Ac objects and tries to perform them.
    Subclasses should override the .acs class member. This class performs
    no action.'''
    acs: Acs = None
    post_acs: Acs = NewState(Completed)
    #blocked_acs: Acs = None
    threshold: float = 0.0
    name: str = None

    def __init__(self, *args, **kwargs):
        self.name = self.__class__.__name__
        super().__init__(*args, **kwargs)

    def on_build(self):
        if not self.action:
            self.action = Ac.as_action(
                as_list(self.acs) + as_list(self.post_acs),
                name=self.name,
                threshold=self.threshold
            )

    def filledattr_always_match(self, name):
        return name == 'action' or super().filledattr_always_match(name)

    def __repr__(self):
        return self.__class__.__name__

#@dataclass
class Persistent(AcNode):
    '''Mix-in for an AcNode that should sleep for a little while and re-run
    after completing its action.'''
    post_acs = [Calm(), Sleep()]

    def on_completion(self):
        '''Don't remove activation edges.'''
        pass

class Restartable(Persistent):
    '''Mix-in for an AcNode that should SleepUntilAwakened after
    successfully completing its action.'''
    post_acs = SleepUntilAwakened()

class LateNoticer(Persistent, AcNode):
    initial_activation = 0.02

class Nonstop(AcNode):
    '''Mix-in for an AcNode that should keep running even after completing its
    action.'''
    post_acs = [
        NewState(Start),
        Calm()
    ]

    # TODO OAOO
    def on_completion(self):
        '''Don't remove activation edges.'''
        pass

class OneShot(AcNode):
    '''Mix-in for an AcNode that should self-destruct after completing its
    action.'''
    post_acs = SelfDestruct()

@dataclass
class AdHocAcNode(AcNode):
    '''An AcNode that accepts the .acs as a ctor argument.'''

    node_params = NodeParams(
        AttrParam('acs', None),
        AttrParam('state', Start),
        MateParam('rm_on_success', 'tags')
    )
