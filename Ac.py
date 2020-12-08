# Ac.py -- Subactions

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence
from util import as_iter, as_list
from Node import Node, NRef, NRefs, MaybeNRef, PortLabels, MaybeCRef
from NodeParams import NodeParams, AttrParam, MateParam
from Action import Action, Actions
from ActiveNode import ActionNode, Start, Completed
from criteria import Criterion, Criteria
from exc import Fizzle, AcNeedArg, AcBlocked, AcFailed
from StdGraph import pg
from util import as_set

AcEnv = dict
'''A binding environment for execution of an Ac.'''

@dataclass
class AcFalse(Fizzle):
    ac: 'Ac'
    actor: NRef
    env: AcEnv

@dataclass
class Ac(ABC):

    def get(self, g: 'G', actor: MaybeNRef, env: AcEnv, name: str) -> Any:
        '''Looks up name, searching first in actor's overrides, then in env,
        then in actor's attrs, and finally in this Ac's attrs.
        Raises AcNeedArg if the value found is None or not found.'''
        try:
            result = g.get_overrides(actor, name)[name]
        except KeyError:
            try:
                result = env[name]
            except KeyError:
                result = g.getattr(actor, name)
                if result is None:
                    try:
                        result = getattr(self, name)
                    except AttributeError:
                        result = None

        if result is None:
            raise AcNeedArg(ac=self, name=name)
        elif isinstance(result, str):
            # TODO Prevent infinite recursion: make sure we haven't tried
            # this key before.
            # TODO What if the value is actually supposed to be a str?
            return self.get(g, actor, env, result)
        else:
            return result

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
        except AcFalse as exc:
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
            ac.go(g, actor, env)

    @classmethod
    def as_action(cls, acs: Union['Ac', Sequence['Ac'], None]) -> Action:
        return AcAction(acs)

Acs = Union[Ac, Iterable[Ac], None]

@dataclass
class AcAction(Action):
    acs: Union[Ac, Sequence[Ac], None]

    def go(self, g, actor):
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
    within: MaybeNRef = None

    def __init__(self, *criteria, within=None):
        self.criteria = criteria
        self.within = within

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        criteria = self.get(g, actor, env, 'criteria')
        within = self.get(g, actor, env, 'within')
        env['nodes'] = g.find_all(*criteria, within=within)

@dataclass
class LookFor(Ac):
    criteria: Criteria = None
    within: MaybeNRef = None
    cond: Acs = None  # Acs to check further criteria

    def __init__(self, *criteria, within=None, cond=None):
        self.criteria = criteria
        self.within = within
        self.cond = cond

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        criteria = self.get(g, actor, env, 'criteria')
        within = self.get(g, actor, env, 'within')
        for node in g.find_all(*criteria, within=within):
            try:
                env['node'] = node
                Ac.call(g, self.cond, actor, env)
                break
            except AcFalse:
                continue
        if not node:
            raise AcFalse(self, actor, env)

@dataclass
class AllAre(Ac):
    criterion: Union[Criterion, None] = None

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        criterion = self.get(g, actor, env, 'criterion')
        nodes = self.get(g, actor, env, 'nodes')
        for node in as_iter(nodes):
            if not criterion(g, node):
                raise AcFalse(self, actor, env)

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
class TagWith(Ac):
    tagclass: MaybeCRef = None
    taggees: NRefs = None
    kwargs: Dict[str, Any] = None

    def __init__(self, *args, **kwargs):
        try:
            self.tagclass = args[0]
            self.taggees = args[1]
        except IndexError:
            pass
        for k, v in kwargs.items():
            setattr(self, k, v)
        self.kwargs = kwargs

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        taggees = self.get(g, actor, env, 'taggees')
        tagclass = self.get(g, actor, env, 'tagclass')
        kwargs = {}
        for k in self.kwargs:
            if k != 'tagclass' and k != 'taggees':
                kwargs[k] = self.get(g, actor, env, k)
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
class AddNode(Ac):
    nodeclass: MaybeCRef = None
    kwargs: Dict[str, Any] = None  # Unconditionally filled by __init__

    def __init__(self, nodeclass, **kwargs):
        self.nodeclass = nodeclass
        self.kwargs = kwargs

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        nodeclass = self.get(g, actor, env, 'nodeclass')
        kwargs = {}
        # TODO Look for required arguments of nodeclass
        for k, v in self.kwargs.items():
            if isinstance(v, str):  # TODO What if the value is property a str?
                # If value is a string, we look it up (indirection)
                kwargs[k] = self.get(g, actor, env, v)
            else:
                kwargs[k] = v
        env['node'] = g.add_node(nodeclass, **kwargs)

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
class Raise(Ac):
    exc: Exception

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        raise self.exc  # TODO How to construct exception object from arguments?

@dataclass
class PrintEnv(Ac):

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        print('ENV', env)

@dataclass
class AcNode(ActionNode):
    '''A node that holds one or more Ac objects and tries to perform them.
    Subclasses should override the .acs class member. This class performs
    no action.'''
    acs: Acs = None

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.name = self.__class__.__name__

    def on_build(self):
        self.action = Ac.as_action(self.acs)

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
