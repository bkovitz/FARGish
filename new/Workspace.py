# Workspace.py -- The workspace in a FARGModel

from dataclasses import dataclass, field, fields, replace, is_dataclass, InitVar
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, \
    Iterator, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable

from FMTypes import Node, WSPred, match_wo_none
from Agent import Agent
from Propagator import Propagator
from Graph import Graph, Hop, WithActivations, GraphPropagatorOutgoing
from inspect import isclass, signature
from util import as_iter, first, force_setattr, clip, sample_without_replacement


@dataclass
class ActivationGraph(WithActivations, Graph):
    propagator: Propagator = field(
        default_factory=lambda: GraphPropagatorOutgoing()
    )

@dataclass
class NodeInWS:
    '''A Node in the Workspace and information about it.'''
    node: Node
    builder: Union[Agent, None]
        # .builder should not be used by Agents; it's strictly for debugging
        # and reporting on model behavor.
    behalf_of: List[Agent] = field(init=False, default_factory=list)
        # .behalf_of is all the Agents that want this Node to exist, including
        # but not limited to its builder.
    tob: int   # time of birth (when Node was added to the ws)
    # activation: float = 1.0
    # overrides for contents?
    # overrides for orientation?

    def add_behalf_of(self, agents):
        self.behalf_of += as_iter(agents)

    def __str__(self):
        return f'{self.elem}  builder={self.builder} tob={self.tob}'

@dataclass
class Workspace:
    '''The Workspace is strictly a container of nodes and the activation
    graph that holds and connects their activations. All notions of Agents
    and Codelets are left to the FARGModel class.'''
    wsd: Dict[Node, NodeInWS] = field(default_factory=dict)
    activation_g: ActivationGraph = field(
        default_factory=ActivationGraph.empty
    )
    t: int = 0  # TODO inherit this from something that knows about time
    mutual_support_weight: float = 1.0
    mutual_antipathy_weight: float = -0.2

    def build(self, *args, **kwargs) -> Node:
        '''The arguments specify an Node to build in the workspace. If such
        an Node already exists, we don't build anything. Returns the built
        or found Node.'''
        if not args:
            raise NotImplementedError('still need to provide object to .build')
        if isclass(args[0]):
            raise NotImplementedError(".build can't yet construct the object for you")
        obj = args[0]
        if obj is None:  # attempting to build None builds nothing
            return None

        builder: Optional[Agent] = kwargs.pop('builder', None)
        niws = self.get_niws(obj)
        if niws is None:  # if the Node is not there, really build it
            niws = self._really_build(obj, builder, **kwargs)
        else:
            obj = niws.node
        if builder:
            niws.add_behalf_of(builder)
            self.add_mutual_support(builder, obj)
        return obj

    def _really_build(
        self, obj: Node, builder: Union[Agent, None], **kwargs
    ) -> NodeInWS:
        init_a = kwargs.pop('init_a', None)
        min_a = kwargs.pop('min_a', None) # only has effect on true build
        max_a = kwargs.pop('max_a', None) # only has effect on true build

        self.wsd[obj] = niws = NodeInWS(obj, builder=builder, tob=self.t)

        # Set up activation stuff
        if init_a is None:
            if builder is None:
                init_a = 1.0
            else:
                init_a = min(1.0, self.a(builder))
        if min_a is not None:
            force_setattr(obj, 'min_a', min_a)
        min_a = getattr(obj, 'min_a', None)
        if max_a is not None:
            force_setattr(obj, 'max_a', max_a)
        max_a = getattr(obj, 'max_a', None)
        init_a = clip(min_a, max_a, init_a)
        self.activation_g.add_node(obj)
        self.activation_g.set_a(obj, init_a)

        return niws

        # create antipathy between obj and its enemies
        """ TODO
        for node in self.nodes(HasAntipathyTo(obj, ignore=builder)):
            self.add_mutual_antipathy(obj, node)
        """

        # TODO Something needs to call an Agent's 'build' codelets
        # Put an .on-build() in this class and call it? FARGModel
        # can override it.

    # Activation / support

    def a(self, node: Node) -> float:
        return self.activation_g.a(node)

    def add_mutual_support(
        self, a: Node, b: Node, weight: Optional[float]=None
    ) -> None:
        if weight is None:
            weight = self.mutual_support_weight
        self.activation_g.add_hop(Hop(a, b, weight))
        self.activation_g.add_hop(Hop(b, a, weight))
        
    # Querying

    def get_niws(self, obj: Node) -> Union[NodeInWS, None]:
        '''Returns obj's NodeInWS from the workspace if it exists, otherwise
        None.'''
        return self.wsd.get(obj, None)
        
    def nodes(self, pred: WSPred=None, es=None) -> Iterable[Node]:
        '''Returns a generator for *all* matches of pred. Unlike .ws_query(),
        .nodes() does not make a weighted choice.'''
        # TODO Explain es. (It's a subset to search.)
        wspred = as_wspred(pred)
        if es is None:
            es = self.wsd.keys()
        return (e for e in as_iter(es) if wspred(self, e))

    def ws_query(self, pred: WSPred, min_a: Union[float, None]=None, k: int=1) \
    -> Iterable[Node]:
        '''Returns generator of up to k nodes that match pred,
        chosen randomly, weighted by activation.'''
        nodes = self.nodes(pred)
        if min_a is not None:
            nodes = (e for e in nodes if self.a(e) >= min_a)
        nodes = list(nodes)
        activations = [self.a(e) for e in nodes]
        yield from sample_without_replacement(
            nodes, weights=activations, k=k
        )

    def the(self, pred: WSPred, es=None) -> Union[Node, None]:
        '''Returns the first element from .nodes(), or None if there isn't
        one.'''
        return first(self.nodes(pred=pred, es=es))

    # Node info

    def builder_of(self, node: Node) -> Union[Agent, None]:
        niws = self.get_niws(node)
        if niws:
            return niws.builder
        else:
            return None

def as_wspred(o: WSPred) -> Callable[[Workspace, Any], bool]:
    '''Returns a predicate function that takes two arguments: a Workspace
    and an object.'''
    # TODO Document the many ways this thing constructs a function.
    if isclass(o):
        return lambda ws, x: isinstance(x, o)  # type: ignore[arg-type]  # mypy bug?
    elif isinstance(o, tuple):
        preds = tuple(as_wspred(p) for p in o)
        return lambda ws, x: any(p(ws, x) for p in preds)
    elif callable(o):
        if first_arg_is_ws(o):
            return o  # type: ignore[return-value]
        else:
            return lambda ws, x: o(x)  # type: ignore[call-arg, operator, misc]  # mypy bug
    elif o is None:
        return lambda ws, x: True
    else:
        return lambda ws, x: match_wo_none(x, o)

def first_arg_is_ws(o: Callable) -> bool:
    p0 = first(signature(o).parameters.values())
    try:
        return issubclass(p0.annotation, Workspace)
    except TypeError:
        return False

