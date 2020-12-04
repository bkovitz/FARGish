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
from exc import Fizzle, AcNeedArg, AcFailure

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
        '''Looks up name, searching first in env and then in this Ac's attrs.
        Raises AcNeedArg if the value found is None or not found.'''
        try:
            result = g.get_overrides(actor, name)[name]
        except KeyError:
            try:
                result = env[name]
            except KeyError:
                try:
                    result = getattr(self, name)
                except AttributeError:
                    result = None

        if result is None:
            raise AcNeedArg(ac=self, name=name)
        else:
            return result

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
        try:
            Ac.run(g, self.acs, actor)
        except AcFailure as exc:
            raise exc.as_action_failure(self, actor)

@dataclass
class All(Ac):
    criterion: Union[Ac, None] = None
    within: MaybeNRef = None

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        criterion = self.get(g, actor, env, 'criterion')
        within = self.get(g, actor, env, 'within')
        result = []
        env['nodes'] = g.find_all(criterion, within=within)

@dataclass
class AllAre(Ac):
    criterion: Criterion

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        criterion = self.get(g, actor, env, 'criterion')
        nodes = self.get(g, actor, env, 'nodes')
        for node in as_iter(nodes):
            if not criterion(g, node):
                raise AcFalse(self, actor, env)

@dataclass
class TagWith(Ac):
    tagclass: MaybeCRef = None

    def go(self, g: 'G', actor: NRef, env: AcEnv) -> None:
        nodes = self.get(g, actor, env, 'nodes')
        tagclass = self.get(g, actor, env, 'tagclass')
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
