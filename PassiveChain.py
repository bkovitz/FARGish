# PassiveChain.py -- PassiveChain and ancillary nodeclasses

from pprint import pprint as pp
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable

from log import *
from exc import Fizzle
from ActiveGraph import pg, pa, pai, paa, ps
from Action import Action
from ActiveNode import ActiveNode, Completed
from Node import CRef, NRef, NRefs, MaybeNRef
from NodeParams import NodeParams, MateParam, AttrParam
from criteria import NodeEq, NotTheArgsOf, And, NotTagged
from util import as_iter, pairwise

# HACK TODO rm
from NumboGraph import DiffIsWantedTagger, NoticeCouldMakeMinus, \
    ProposeDoingNoticedOperation

class StartPassiveChainRunner(Action):

    def go(self, g, actor):
        # Find initial node.
        # Find/choose who triggered it.
        # Build a LiveActiveChain with focal point on the trigger
        # unless it already exists.
        initial_node = g.initial_member_of(actor)
        if not initial_node:
            raise Fizzle
        initial_node = g.as_node(initial_node)
        # TODO It would be nice if you could pass a nodeid directly to
        # NodeEq. The current way, requiring a Node, is bug-prone (but
        # needed for unit tests, at the least).
        triggering_node = g.look_for(
            NodeEq(initial_node), #OfClass(initial_node),
            tupcond=NotTheArgsOf('LiveActiveChain', 'focal_point'),
            subset=g.neighbors(initial_node, 'activation_from')
        )
        if not triggering_node:
            raise Fizzle
        lac = make_live_active_chain(
            g,
            actor,
            triggering_node,
            member_of=g.containers_of(triggering_node)
        )
        if not lac:
            raise Fizzle('could not make LiveActiveChain')
        g.excite(actor, lac)
        g.set_activation_from_to(triggering_node, lac)
        g.calm(actor)

# Custom ActiveNodes

#class PassiveChain(ActiveNode, Group):
class PassiveChain(ActiveNode):
    # TODO inherit from Group and rm this node_params.
    node_params = NodeParams(MateParam('members', 'member_of'))

    def actions(self):
        # TODO Only if there is an actual need to start a runner
        return StartPassiveChainRunner()

def make_live_active_chain(
    g: 'G', source_chain: NRef, focal_point: NRefs, **kwargs
) -> NRef:
    if not source_chain:
        raise UnexpectedFizzle('no source passive_chain')
    if not focal_point:
        raise UnexpectedFizzle('no focal_point')
    initial_source_node = g.initial_member_of(source_chain)
    if not initial_source_node:
        raise UnexpectedFizzle('no initial_source_node')
    live_active_chain = g.add_node(
        'LiveActiveChain',
        source_chain=source_chain,
        focal_point=focal_point,
        **kwargs
    )
    prev_active_node: MaybeNRef = None
    for (prev_source_node, next_source_node) in pairwise(
        g.walk(initial_source_node, 'next')
    ):
        next_active_node_class = look_up_active_node_class(
            g, prev_source_node, next_source_node
        )
        new_active_node = g.add_node(
            next_active_node_class,
            focal_point=focal_point,
            need_basis_like=prev_source_node,
            prev=prev_active_node,  # TODO already_built should ignore this
            member_of=live_active_chain, # TODO   "          "        "
            support_from=live_active_chain
        )
        g.set_mutual_activation(live_active_chain, new_active_node)
        prev_active_node = new_active_node
        
    return live_active_chain

def look_up_active_node_class(
    g, current_live_node, next_source_node
) -> CRef:
    # HACK
    #print(f'LOOKUP ANC {g.nodestr(current_live_node)}, {g.nodestr(next_source_node)}')
    
    fromclass = g.as_nodeclass(current_live_node)
    toclass = g.as_nodeclass(next_source_node)
    return anc_dict[(fromclass.__name__, toclass.__name__)]

# HACK
anc_dict = {
    ('Diff', 'DiffIsWanted'): DiffIsWantedTagger,
    ('DiffIsWanted', 'Minus'): NoticeCouldMakeMinus,
    ('Minus', 'Proposal'): ProposeDoingNoticedOperation
}

def make_passive_chain(
    g: 'G', *nodespecs: Iterable['Node'], **kwargs
):
    '''You will probably want to provide member_of=g.ws or
    member_of=g.slipnet.'''
    pchain = g.add_node(PassiveChain, **kwargs)
    members = [
        g.add_node(nodespec, member_of=pchain) for nodespec in nodespecs
    ]
    g.link_sequence(members)
    g.set_mutual_activation(pchain, members)
