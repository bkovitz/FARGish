# Ac.py -- Subactions

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence
from util import as_iter, as_list
from Node import Node, NRef, NRefs, MaybeNRef, PortLabels, MaybeCRef
from Action import Action, Actions
from ActiveNode import ActionNode

class AcEnv(dict):
    '''A binding environment for execution of an Ac.'''
    pass

@dataclass
class Ac(ABC):

    def get(self, env: AcEnv, name: str) -> Any:
        if name in env:
            return env[name]
        else:
            return getattr(self, name)  # TODO Catch AttributeError

class AcBool(Ac):
    '''An Ac that does a test.'''
    pass

# NEXT Better idea: Every Ac returns True to continue and False to stop.

empty_env = AcEnv()

@dataclass
class OfClass(AcBool):
    nodeclass: MaybeCRef = None

    def do(self, g: 'ActiveGraph', node: MaybeNRef, env: AcEnv) \
    -> Tuple[AcEnv, bool]:
        return env, g.is_of_class(node, self.nodeclass)

@dataclass
class Tagged(AcBool):
    tagclass: MaybeNRef = None

    def do(self, g: 'ActiveGraph', node: MaybeNRef, env: AcEnv) \
    -> Tuple[AcEnv, bool]:
        return env, g.has_tag(node, self.tagclass)

@dataclass
class All(Ac):
    criterion: Union[Ac, None] = None
    within: MaybeNRef = None

    def do(self, g: 'ActiveGraph', actor: NRef, env: AcEnv) \
    -> Tuple[AcEnv, NRefs]:
        result = []
        for node in g.members_recursive(self.within):
            env, tf = self.criterion.do(g, node, env)
            if tf:
                result.append(node)
        env['nodes'] = result
        return env, result

@dataclass
class AllAre(AcBool):
    criterion: Union[Ac, None] = None

    def do(self, g: 'ActiveGraph', node: MaybeNRef, env: AcEnv) \
    -> Tuple[AcEnv, bool]:
        criterion = self.get(env, 'criterion')
        nodes = self.get(env, 'nodes')
        tf = False
        for node in as_iter(nodes):
            env, tf = criterion.do(g, node, env)
            if not tf:
                break
        return env, tf

@dataclass
class TagWith(Ac):
    tagclass: MaybeCRef = None

    def do(self, g: 'ActiveGraph', node: MaybeNRef, env: AcEnv) \
    -> Tuple[AcEnv, bool]:
        nodes = self.get(env, 'nodes')
        tagclass = self.get(env, 'tagclass')
        tag = g.add_tag(tagclass, nodes)
        env['tag'] = tag
        return env, tag

@dataclass
class Acs(Ac):
    '''A sequence of Acs.'''
    acs: Sequence[Ac]

    def __init__(self, *acs: Ac):
        self.acs = acs

    def do(self, g: 'ActiveGraph', actor: NRef, env: AcEnv) -> Actions:
        result = None
        for ac in self.acs:
            env, result = ac.do(g, actor, env)
        return env, result
