# Ac.py -- Subactions

from abc import ABC, abstractmethod
from dataclasses import dataclass, field, fields, replace
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence
from contextlib import contextmanager

from Node import Node, NRef, NRefs, MaybeNRef, PortLabels, MaybeCRef
from NodeParams import NodeParams, AttrParam, MateParam
from Action import Action, Actions
from ActiveNode import ActionNode, Start, Completed
from criteria import Criterion, Criteria, OfClass
from exc import Fizzle, AcNeedArg, AcBlocked, AcFailed, AcError, FargDone
from StdGraph import Context, MyContext, pg
from util import as_set, Quote, as_iter, as_list, is_seq_of

AcEnv = dict
'''A binding environment for execution of an Ac.'''

@dataclass
class AcFalse(Fizzle):
    ac: 'Acs'
    actor: NRef
    env: AcEnv

@dataclass
class AcFizzle(Fizzle):
    '''Exception indicating that an Acs should fail silently.'''
    pass

@dataclass
class AcCantFind(AcFailed):
    '''We looked for a node meeting certain criteria and didn't find one.'''
    criteria: Criteria

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
        Raises AcNeedArg if the value found is None or not found.'''
        # TODO Document name_overrides_
        # NEXT Find 'opwithin' in LookFor.within
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
            raise AcNeedArg(ac=self, name=name)
        elif isinstance(result, str):
            # TODO Prevent infinite recursion: make sure we haven't tried
            # this key before.
            # TODO What if the value is actually supposed to be a str?
            #print('GETRECUR', self, actor, name, result)
            result = self.get(g, actor, env, result)
            #print('GETRECUR2', repr(result))
            return result
        elif Context.is_context(result):
            result = result.within(g, actor)
            #print('GETCTX', result)
            return result
        elif is_seq_of(result, Criterion):
            return [self.fix_criterion(g, c, actor, env) for c in result]
        elif isinstance(result, Criterion):
            return self.fix_criterion(g, result, actor, env)
        else:
            return Quote.get(result)

    def fix_criterion(
        self, g: 'G', criterion: Criterion, actor: MaybeNRef, env: AcEnv
    ) -> Criterion:
        #print('FIXC', criterion, env)
        changes = {}
        for field_name in (f.name for f in fields(criterion)):
            oldv = getattr(criterion, field_name)
            newv = self.get(g, actor, env, field_name, default=oldv)
            #print('FIXLOOP', actor, field_name, oldv, newv)
            if newv is not None and newv != oldv:
                changes[field_name] = newv
        result = replace(criterion, **changes)
        #print('GETCR', criterion, changes, result)
        return result

    def get_or_fizzle(self, g: 'G', actor: MaybeNRef, env: AcEnv, name: str) \
    -> Any:
        '''Same as .get() but raises AcFizzle if can't find value for 'name'.'''
        try:
            return self.get(g, actor, env, name)
        except AcNeedArg:
            raise AcFizzle

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
        This makes run() suitable for run acs as an Action, but not as a
        subroutine called from inside an Ac. For that, see Ac.call().'''
        env = AcEnv()
        try:
            cls.call(g, acs, actor, env)
        except AcFizzle:
            pass
        except AcFalse as exc:
            #print('RUN', exc.__class__, exc)
            return exc.env
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
            except (AcFizzle, AcFalse, AcFailed, AcBlocked, FargDone):
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
        actor = g.as_node(actor)
        try:
            Ac.run(g, self.acs, actor)
        except AcBlocked as exc:
            raise exc.as_action_blocked(self, actor)
        except AcFailed as exc:
            raise exc.as_action_failure(self, actor)
        actor.state = Completed

@dataclass
class All(Ac):
    criteria: Acs = None
    within: MaybeNRef = None   #TODO Allow MyContext

    def __init__(self, *criteria, within=None):
        self.criteria = criteria
        self.within = within

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        criteria = as_list(self.get(g, actor, env, 'criteria'))
        within = self.get(g, actor, env, 'within')
        env['nodes'] = g.find_all(*criteria, within=within)

@dataclass
class LookFor(Ac):
    criteria: Criteria = None
    within: MaybeNRef = None
    cond: Acs = None  # Acs to check further criteria
    asgn_to: str = None

    def __init__(self, *criteria, within=None, asgn_to='node', cond=None):
        self.criteria = criteria
        self.within = within
        self.cond = cond
        self.asgn_to = asgn_to

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        criteria = as_list(self.get(g, actor, env, 'criteria'))
        within = self.get(g, actor, env, 'within')
        node = None
        #print('LNODE0', node, criteria, within)
        for node in g.find_all(*criteria, within=within):
            #print('LNODE', node, criteria, within)
            try:
                env['node'] = node
                Ac.call(g, self.cond, actor, env)
                break
            except AcFalse:
                continue
        else: # TODO UT that exercises this 'else'
            node = None
        if not node:
            raise AcFalse(self, actor, env)
        else:
            env[self.asgn_to] = node

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
    within: MaybeNRef = None

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        within = self.get(g, actor, env, 'within')
        env['nodes'] = g.members_of(within)

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
        env['node'] = g.add_node(nodeclass, **kwargs)

@dataclass
class FindParamName(Ac):

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        problem = self.get(g, actor, env, 'problem')
        # TODO violates Law of Demeter (for Nodes)
        reason = g.getattr(problem, 'reason')
        env['name'] = Quote(reason.name)

@dataclass
class LookForArg(Ac):
    within: MaybeNRef = MyContext  # TODO type hint should allow MyContext

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        within = self.get(g, actor, env, 'within')
        #name = self.get(g, actor, env, 'name')
        # TODO Determine the class from 'name'
        criteria = OfClass('Glom')  # HACK
        node = g.look_for(criteria, within=within)  # HACK
        if not node:
            #print('LOOKCANT', criteria, node, within)
            raise AcCantFind(self, actor, criteria)
        env['node'] = node

@dataclass
class AddOverride(Ac):

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        behalf_of = self.get(g, actor, env, 'behalf_of')
        node = self.get(g, actor, env, 'node')
        name = self.get(g, actor, env, 'name')
        g.add_override_node(behalf_of, name, node)
    
@dataclass
class RemoveBlockedTag(Ac):

    def go(self, g, actor, env):
        problem = self.get_or_fizzle(g, actor, env, 'problem')
        g.remove_node(problem)
        
@dataclass
class OrFail(Ac):
    ac: Ac
    exc: AcFailed

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        try:
            self.ac.go(g, actor, env)
        except AcFalse:
            raise self.exc(self.ac, actor)

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
class PrintEnv(Ac):
    '''For debugging.'''

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        print('ENV', env)

@dataclass
class AcNode(ActionNode):
    '''A node that holds one or more Ac objects and tries to perform them.
    Subclasses should override the .acs class member. This class performs
    no action.'''
    acs: Acs = None
    blocked_acs: Acs = None
    threshold: float = 0.0

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.name = self.__class__.__name__

    def on_build(self):
        if not self.action:
            self.action = Ac.as_action(
                self.acs, name=self.name, threshold=self.threshold
            )

    def __repr__(self):
        return self.__class__.__name__

@dataclass
class AdHocAcNode(AcNode):
    '''An AcNode that accepts the .acs as a ctor argument.'''

    node_params = NodeParams(
        AttrParam('acs', None),
        AttrParam('state', Start),
        MateParam('rm_on_success', 'tags')
    )
