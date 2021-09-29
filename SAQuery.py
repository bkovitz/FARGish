# SAQuery.py -- Functions for querying a spreading-activation network

from Graph2 import GraphPropagatorOutgoing
from util import as_iter, as_dict, union, pts, pr


@dataclass
class TyrrellPropagator(GraphPropagatorOutgoing):
    # The formulas that adjust incoming weights come from p. 206 of Toby
    # Tyrrell's doctoral dissertation.
    tyrrell_alpha: float = 0.05
        # constant for reducing influence of edge multiplicity: positive inputs
    tyrrell_beta: float = 0.05
        # constant for reducing influence of edge multiplicity: negative inputs

    def propagate_once(self, g, old_d):
        # Decay.
        new_d: Dict[Node, float] = defaultdict(float,
            ((node, self.clip_a(g, node, a * self.alpha))
                for node, a in old_d.items()
            )
        )
        # TODO Remove nodes with a < epsilon

        # Find max incoming activation and sum of incoming activations for
        # each node.
        maxin_d: Dict[Node, float] = defaultdict(float) # positive influences
        possumin_d: Dict[Node, float] = defaultdict(float)
        minin_d: Dict[Node, float] = defaultdict(float) # negative influences
        negsumin_d: Dict[Node, float] = defaultdict(float)

        for delta in self.make_deltas(g, old_d):
            amt = (
                delta.amt
                * (1.0 + self.positive_feedback_rate
                         * old_d.get(delta.nodeid, 0.0))
                * (1.0 - self.alpha)
            )
            '''
            if delta.nodeid == Before(7) and delta.amt < 0.0:
                print()
                print('DE', delta, '   ', amt)
                print()
            '''
            if amt >= epsilon:
                maxin_d[delta.nodeid] = max(maxin_d[delta.nodeid], amt)
                possumin_d[delta.nodeid] += amt
            elif amt <= -epsilon:
                minin_d[delta.nodeid] = min(minin_d[delta.nodeid], amt)
                negsumin_d[delta.nodeid] += amt

        # Apply the Tyrrell averages of the deltas

        for node in union(maxin_d.keys(), minin_d.keys()):
            #print('PR', node, maxin_d.get(node, 0.0), possumin_d.get(node, 0.0), minin_d.get(node, 0.0), negsumin_d.get(node, 0.0))
            '''
            print('PR1', node, minin_d.get(node, 0.0), negsumin_d.get(node, 0.0), (
                (minin_d.get(node, 0.0)
                  + self.tyrrell_beta * negsumin_d.get(node, 0.0))
                /
                (1 + self.tyrrell_beta)
            ))
            '''
            new_a = new_d[node] + (
                (maxin_d.get(node, 0.0)
                  + self.tyrrell_alpha * possumin_d.get(node, 0.0))
                /
                (1 + self.tyrrell_alpha)
            ) + (
                (minin_d.get(node, 0.0)
                  + self.tyrrell_beta * negsumin_d.get(node, 0.0))
                /
                (1 + self.tyrrell_beta)
            )
            new_d[node] = self.clip_a(g, node, new_a)
            # TODO Record this in self.flows?

        return self.normalize(new_d)

@dataclass(frozen=True)
class NodeA:
    '''Node and activation.'''
    node: Node
    a: float

    def __str__(self):
        try:
            nodestr = self.node.__name__
        except AttributeError:
            nodestr = str(self.node)
        return f'{nodestr:20s} {self.a:2.5f}'

def slipnet_dquery(
    g: Graph,
    p: Propagator,
    features: Iterable[Hashable]=None,
    activations_in: Dict[Hashable, float]=None
) -> Dict[Hashable, float]:
    '''Pass either features or a dictionary of activations.
    Returns dictionary of activations.'''
    if activations_in is None:
        activations_in = {}
        for f in as_iter(features):
            if isinstance(f, NodeA):
                a = f.a
                f = f.node
            else:
                a = 1.0
            activations_in[f] = a
    #print('DQ', type(activations_in))
    return p.propagate(g, activations_in)

def topna(
    d: Dict[Node, float],
    type: Type=None,
    k: Union[int, None]=1,
    filter: Union[Callable, None]=None
) -> List[NodeA]:
    '''Returns a list of the top k nodes in d, by activation, restricted to
    nodes of 'type' and that pass 'filter'.'''
    if filter is None:
        filter = lambda x: True
    if type is None:
        nas = [
            NodeA(node, a)
                for (node, a) in d.items()
                    if filter(node)
        ]
    else:
        nas = [
            NodeA(node, a)
                for (node, a) in d.items()
                    if isinstance(node, type) and filter(node)
        ]
    if k is None:
        return sorted(nas, key=attrgetter('a'), reverse=True)
    else:
        return nlargest(k, nas, key=attrgetter('a'))

def top(*args, **kwargs) -> List[Node]:
    return [na.node for na in topna(*args, **kwargs)]

def top1(*args, **kwargs) -> Union[Node, None]:
    try:
        return top(*args, **kwargs)[0]
    except IndexError:
        return None

