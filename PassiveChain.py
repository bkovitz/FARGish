# PassiveChain.py -- PassiveChain and ancillary nodeclasses

from pprint import pprint as pp

from log import *
from exc import Fizzle
from ActiveGraph import pg, pa, pai, paa, ps
from Action import Action
from ActiveNode import ActiveNode, Completed
from Node import CRef
from NodeParams import NodeParams, MateParam, AttrParam
from criteria import NodeEq, NotTheArgsOf
from util import as_iter

# HACK TODO rm
from NumboGraph import DiffIsWantedTagger, NoticeCouldMakeMinus, \
    ProposeDoingNoticedOperation

class StartPassiveChainRunner(Action):

    def go(self, g, actor):
        # Find initial node.
        # Find/choose who triggered it.
        # Build a RunPassiveChain with focal point on the trigger
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
            tupcond=NotTheArgsOf(RunPassiveChain, 'triggering_node'),
            subset=g.neighbors(initial_node, 'activation_from')
        )
        if not triggering_node:
            raise Fizzle
        runner = g.add_node(
            #RunPassiveChain,
            PassiveChainRunner,
            passive_chain=actor,
            focal_point=triggering_node,
            current_live_node=triggering_node,
            current_source_node=initial_node,
            member_of=g.containers_of(triggering_node)
        )
        g.boost_activation_from_to(actor, runner)
        g.calm(actor)

# Custom ActiveNodes

#class PassiveChain(ActiveNode, Group):
class PassiveChain(ActiveNode):
    # TODO inherit from Group and rm this node_params.
    node_params = NodeParams(MateParam('members', 'member_of'))

    def actions(self):
        # TODO Only if there is an actual need to start a runner
        return StartPassiveChainRunner()

class PassiveChainRunner(ActiveNode):

    node_params = NodeParams(
        MateParam('focal_point', 'tags'),
        MateParam('passive_chain', 'tags')
    )

    def actions(self):
        return RunPassiveChain()

class RunPassiveChain(Action):

    def go(self, g, actor):
        # Need to know: Archetypal PassiveChain; initial workspace node
        # Find current passive node
        # Has the following node been built?
        # No: build it
        # Yes: sleep
        # No following node in archetype: Completed
        # TODO Fail when we wait too long.
        focal_point = g.neighbors(actor, 'focal_point')
        current_source_node = g.neighbor(actor, 'current_source_node')
        next_source_node = g.neighbor(current_source_node, 'next')
        current_live_node = g.neighbor(actor, 'current_live_node')
        current_active_node = g.neighbor(actor, 'current_active_node')

        #print('RUNPASSIVECHAIN', actor, ' ', g.nodestr(current_live_node))

        if not focal_point:
            g.unexpected_abort(actor, 'no focal_point')
        elif not current_source_node:
            g.unexpected_abort(actor, 'no current_source_node')
        elif not next_source_node:
            g.unexpected_abort(actor, 'no next_source_node')
            # If there's no remaining source node to build an analog of, then
            # we're done.
            #g.new_state(actor, Completed)
        elif not current_live_node:
            # No current_live_node => we're just starting up.
            # We take the focal_point as the node that triggered actor to be
            # built, so we 
            if g.is_of_class(focal_point, current_source_node):
                g.add_edge(
                    actor,
                    'current_live_node',
                    focal_point,
                    'tags'  # TODO omit 'tags' when .add_edge can handle it
                )
            else:
                fp = g.nodestr(focal_point)
                csn = g.nodestr(current_source_node)
                g.unexpected_abort(
                    actor,
                    f'attempted to start but focal_point {fp}' +
                    f' not an instance of current_source_node {csn}'
                )
        elif not current_active_node:
            # Make agent to produce the analog of the next_source_node
            self.start_new_active_node(
                g, actor, current_live_node, next_source_node, focal_point,
                current_active_node
            )
        else:
            # There is a current_active_node running. Let's see if the next
            # node in the live chain (not necessarily by the
            # current_active_node).
            next_live_node = g.neighbor(
                current_active_node,
                port_label='completion',
                neighbor_class=next_source_node
            )
            if next_live_node:
                # Yes: the current_active_node has built the next step in
                # the new chain, or something else built it.
                next_next_source_node = g.neighbor(next_source_node, 'next')
                if not next_next_source_node:  # Are we done?
                    g.new_state(actor, Completed)
                else: # No, so advance to the next step in the source chain
                    g.move_edge(
                        actor,
                        'current_source_node',
                        next_source_node,
                        'tags'
                    )
                    g.move_edge(
                        actor,
                        'next_source_node',
                        next_next_source_node,
                        'tags'
                    )
                    g.move_edge(
                        actor,
                        'current_live_node',
                        next_live_node,
                        'tags'
                    )
                    self.start_new_active_node(
                        g, actor, next_live_node, next_next_source_node,
                        focal_point, current_active_node
                    )
            else:
                # No: we're waiting for the current_active_node to build the
                # next node in the live chain.
                # TODO Fail if current_active_node Failed.
                # TODO Fail or something if we've waited too long.
                g.sleep(actor)

    def start_new_active_node(
        self, g, actor, current_live_node, next_source_node, focal_point,
        current_active_node
    ):
        g.remove_hops_from_port(actor, 'current_active_node')
        next_active_node_class = self.look_up_active_node_class(
            g,
            actor,
            current_live_node,
            next_source_node
        )
        new_active_node = g.add_node(
            next_active_node_class, focal_point=focal_point
        )
        g.excite(actor, new_active_node)
        g.set_support_from_to(actor, new_active_node, 1.0)
        g.set_mutual_activation(actor, new_active_node)
        g.cut_off_support(actor, current_active_node)
        g.add_edge(
            actor, 'current_active_node', new_active_node, 'behalf_of'
        )
        g.sleep(actor)

    def look_up_active_node_class(
        self, g, actor, current_live_node, next_source_node
    ) -> CRef:
        # HACK
        #print(f'LOOKUP ANC {g.nodestr(current_live_node)}, {g.nodestr(next_source_node)}')
        
        fromclass = g.as_nodeclass(current_live_node)
        toclass = g.as_nodeclass(next_source_node)
        return self.anc_dict[(fromclass.__name__, toclass.__name__)]

    # HACK
    anc_dict = {
        ('Diff', 'DiffIsWanted'): DiffIsWantedTagger,
        ('DiffIsWanted', 'Minus'): NoticeCouldMakeMinus,
        ('Minus', 'Proposal'): ProposeDoingNoticedOperation
    }
