# ChainSA.py -- Experiment to chain and augment spreading-activation networks

from dataclasses import dataclass, field
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence
from inspect import isclass

from Slipnet import Slipnet as BaseSlipnet, IntFeatures, FeatureWrapper
from testSlipnet import Equation, plus, times, minus, Before, After, Operator
from util import pr, pts, is_iter, as_iter

class Slipnet(IntFeatures, BaseSlipnet):
    pass

# TODO
"""
Slipnet.__str__ or visualize.  DONE

Make a simple slipnet with arithmetic for {1..20}{+-x}{1..20}.  DONE

Make an extension slipnet with competition for blanks: [a] [op] [b] = [c],
[c] [op] [d] = [e].
    Rig up a function that calls the slipnet with two dictionaries.

Pulse it with {a,b,c}={4,5,6}, d=15.

"""

slipnet = Slipnet(
    Equation((a, b), operator, operator.call(a, b))
        for a in range(1, 20)
        for b in range(1, 12)
        for operator in [plus, times, minus]
        if a >= b
)
def mut_inh(pred):
    if is_iter(pred):
        nodes1 = nodes2 = pred
    else:
        nodes1 = slipnet.qnodes(pred)
        nodes2 = slipnet.qnodes(pred)
    for n1 in nodes1:
        for n2 in nodes2:
            if n1 != n2:
                slipnet.add_edge(n1, n2, weight=-0.2)
mut_inh(Before)
mut_inh(After)
mut_inh(int)
mut_inh(Operator)
mut_inh(Equation)

def q(features, k=20, type=Equation):
    return slipnet.query(features, k=k, type=type)

@dataclass(frozen=True)
class Blank(FeatureWrapper):
    index: int=None   # which Blank

    def __str__(self):
        return f'Blank({self.feature}, {self.index})'
    
augnodes = [  # nodes with which to augment the slipnet
    Blank(Before(n), i)
        for n in [4, 5, 6]
            for i in range(3)
]
slipnet.add_layer2_nodes(augnodes)
sm = set(
    frozenset([a, b])
        for a in augnodes
        for b in augnodes
        if a != b and a.feature == b.feature
)
mut_inh(sm)

#pr(slipnet)
args456 = [Before(4), Before(5), Before(6), After(15)]
pr(q(args456))
##pr(q(args456, type=Blank))

args = [Before(4), Before(5), After(15)]
#pts(q(args, type=Blank))
#pr(augnodes)
#print()
#pr(sm)
#pr(slipnet.qnodes(Blank))


