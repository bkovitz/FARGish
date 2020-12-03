# Ac.py -- Subactions

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence
from util import as_iter, as_list
from Node import Node, NRef, NRefs, MaybeNRef, PortLabels, MaybeCRef
from NodeParams import NodeParams, AttrParam, MateParam
from Action import Action, Actions
from ActiveNode import ActionNode, Start
from criteria import Criterion
from exc import Fizzle

class AcEnv(dict):
    '''A binding environment for execution of an Ac.'''
    pass

@dataclass
class AcFalse(Fizzle):
    ac: 'Ac'
    actor: NRef
    env: AcEnv

@dataclass
class Ac(ABC):

    def get(self, env: AcEnv, name: str) -> Any:
        if name in env:
            return env[name]
        else:
            return getattr(self, name)  # TODO Catch AttributeError

    @abstractmethod
    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        '''Do the partial Action defined by this Ac. Raise an ActionFailed
        exception if missing an argument.'''
        pass

    @classmethod
    def run(
        cls,
        g: 'ActiveGraph',
        acs: Union['Ac', Sequence['Ac'], None],
        actor: NRef
    ) -> AcEnv:
        '''Runs acs, starting from an empty AcEnv.'''
        env = AcEnv()
        try:
            for ac in as_iter(acs):
                ac.go(g, actor, env)
        except AcFalse as exc:
            return exc.env
        return env

    @classmethod
    def as_action(cls, acs: Union['Ac', Sequence['Ac'], None]) -> Action:
        return AcAction(acs)

@dataclass
class AcAction(Action):
    acs: Union[Ac, Sequence[Ac], None]

    def go(self, g, actor):
        Ac.run(g, self.acs, actor)

@dataclass
class All(Ac):
    criterion: Union[Ac, None] = None
    within: MaybeNRef = None

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        criterion = self.get(env, 'criterion')
        within = self.get(env, 'within')
        result = []
        env['nodes'] = g.find_all(criterion, within=within)

@dataclass
class AllAre(Ac):
    criterion: Criterion

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        criterion = self.get(env, 'criterion')
        nodes = self.get(env, 'nodes')
        for node in as_iter(nodes):
            if not criterion(g, node):
                raise AcFalse(self, actor, env)

@dataclass
class TagWith(Ac):
    tagclass: MaybeCRef = None

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        nodes = self.get(env, 'nodes')
        tagclass = self.get(env, 'tagclass')
        tag = g.add_tag(tagclass, nodes)
        env['result'] = tag

class AcNode(ActionNode):
    '''A node that holds one or more Ac objects and tries to perform them.'''
    node_params = NodeParams(
        AttrParam('acs'),
        AttrParam('state', Start),
        MateParam('rm_on_success', 'tags')
    )

    def on_build(self):
        self.action = Ac.as_action(self.acs)
