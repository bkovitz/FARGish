# numbo6.py

from pprint import pprint as pp
from itertools import chain

from NumboGraph import *
from log import *
from exc import *
from ActiveGraph import pg, pa, pai, paa, ps
from ActiveNode import ActionNode
from criteria import NotTagged, TupAnd as CTupAnd

# Custom exceptions

@dataclass
class NeedOperands(FizzleAndBlock):

    agent_nodeclass = 'LookForOperands'

# Custom AcNodes

class LookForOperands(OneShot, AcNode):
    # TODO If no operands, raise an alarm
    acs = [
        All(CTagged(Avail), focal_point=MyContext),
        Boost(),
        RemoveBlockedTag()
    ]

class LateNoticer(Persistent, AcNode):
    initial_activation = 0.02

class NoticeCouldMakePlus(LateNoticer):
    min_support_for = 1.0
    acs = [
        OrBlock(
            LookForTup(
                [And(CTagged(Avail), MinActivation(3.0)),
                 And(CTagged(Avail), MinActivation(3.0))],
                tupcond=NotTheArgsOf(Plus, 'source'),
                focal_point=InWorkspace,
            ),
            NeedOperands.from_env()
        ),
        BuildOpResult(Plus, operands='nodes')
    ]

class NoticeCouldMakeTimes(LateNoticer):
    min_support_for = 1.0
    acs = [
        OrBlock(
            LookForTup(
                [And(CTagged(Avail), MinActivation(3.0)),
                 And(CTagged(Avail), MinActivation(3.0))],
                tupcond=NotTheArgsOf(Times, 'source'),
                focal_point=InWorkspace,
            ),
            NeedOperands.from_env()
        ),
        BuildOpResult(Times, operands='nodes')
    ]

class NoticeCouldMakeMinus(LateNoticer):
    min_support_for = 1.0
    acs = [
        OrBlock(
            LookForTup(
                [And(CTagged(Avail), MinActivation(3.0)),
                 And(CTagged(Avail), MinActivation(3.0))],
                tupcond=CTupAnd(
                    NotTheArgsOf(Minus, 'source'),
                    GreaterThan()
                ),
                focal_point=InWorkspace,
            ),
            NeedOperands.from_env()
        ),
        DeTup(asgn_to=('minuend', 'subtrahend')),
        BuildOpResult(
            Minus,
            minuend='minuend',
            subtrahend='subtrahend',
            completion_of='this'
        )
    ]

class ProposeDoingNoticedOperation(Persistent, AcNode):
    acs = [
        LookFor(
            OperatorWithAvailOperands(),
            focal_point=InWorkspace,
            asgn_to='operator'
        ),
        #AsgnNeighbors(node='operator', port_label=Quote('operands')),
        AsgnProposedNeighbors(node='operator', port_label=Quote('operands')),
        PrintEnv(),
        AddNode(
            Proposal,
            action=ConsumeOperands(),
            #proposed_operands='operands',
            neighbors='proposed',
                # HACKish: neighbors is a dict; handled specially by AddNode
            proposed_operator='operator',
            completion_of='this'
        ),
        Boost(nodes='node')
    ]

# Custom Actions

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

        print('RUNPASSIVECHAIN', actor, ' ', g.nodestr(current_live_node))

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
            # TODO Support, too
            g.set_mutual_activation(actor, new_active_node)
            g.cut_off_support(actor, current_active_node)
            g.add_edge(
                actor, 'current_active_node', new_active_node, 'behalf_of'
            )
            g.sleep(actor)
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
                    g.remove_hops_from_port(actor, 'current_active_node')
            else:
                # No: we're waiting for the current_active_node to build the
                # next node in the live chain.
                # TODO Fail if current_active_node Failed.
                # TODO Fail or something if we've waited too long.
                g.sleep(actor)

    def look_up_active_node_class(
        self, g, actor, current_live_node, next_source_node
    ) -> CRef:
        # HACK
        print(f'LOOKUP ANC {g.nodestr(current_live_node)}, {g.nodestr(next_source_node)}')
        
        fromclass = g.as_nodeclass(current_live_node)
        toclass = g.as_nodeclass(next_source_node)
        return self.anc_dict[(fromclass.__name__, toclass.__name__)]

    # HACK
    anc_dict = {
        ('Diff', 'DiffIsWanted'): DiffIsWantedTagger,
        ('DiffIsWanted', 'Minus'): NoticeCouldMakeMinus,
        ('Minus', 'Proposal'): ProposeDoingNoticedOperation
    }

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

class PassiveChain(ActiveNode, Group):

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

class Numbo6Graph(NumboGraph):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.max_actions = 2
        self.add_nodeclasses(LookForOperands)

    def make_initial_nodes(self):
        super().make_initial_nodes()
        target = self.look_for(Target, focal_point=self.ws)
        want = self.tag_of(target, Want)
        assessor = self.add_node(AssessProposal, behalf_of=want)
        self.set_support_from_to(want, assessor, 1.0)
        ncmp = self.add_node(NoticeCouldMakePlus, member_of=self.ws)
        ncmt = self.add_node(NoticeCouldMakeTimes, member_of=self.ws)
        #ncmm = self.add_node(NoticeCouldMakeMinus, member_of=self.ws)
        pdno = self.add_node(ProposeDoingNoticedOperation, member_of=self.ws)
        difft = self.add_node(DiffTagger, member_of=self.ws)
        diwt = None #self.add_node(DiffIsWantedTagger, member_of=self.ws)
        oot = self.add_node(OoMTagger, member_of=self.ws)
        oogtt = self.add_node(OoMGreaterThanTagger, member_of=self.ws)
        oo1bt = self.add_node(OoMSmallGapToWantedTagger, member_of=self.ws)
        oobigt = self.add_node(OoMBigGapToWantedTagger, member_of=self.ws) 
        nsolved = self.look_for(NoticeSolved)
        self.add_activation_autolinks(
            (OoMSmallGapToWanted, ncmp),
            #(OperandBelowWanted, ncmp),
            (OoMBigGapToWanted, ncmt),
            (OoMGreaterThan, [oo1bt, oobigt]),
            (OoM, [oogtt, oo1bt, oobigt]),
            (Diff, diwt),
            #(DiffIsWanted, ncmm),
            (Number, [difft, oot]),
            (Avail, nsolved),
            (Operator, pdno)  # TODO Only "noticed" Operators
        )

    def make_slipnet(self):
        sl = self.add_node(Slipnet)
        pc1 = self.add_node(PassiveChain, member_of=sl)
        n1 = self.add_node(Diff, value=1, member_of=pc1)
        n2 = self.add_node(DiffIsWanted, member_of=pc1)
        n3 = self.add_node(Minus, member_of=pc1)
        n4 = self.add_node(Proposal, member_of=pc1)
        self.link_sequence([n1, n2, n3, n4])
        self.set_mutual_activation(pc1, [n1, n2, n3, n4])
        self.set_activation(self.members_recursive(sl), 0.0)

    def end_of_timestep(self):
        super().end_of_timestep()
        self.link_activation_to_archetypes(self.new_nodes)

    def allowable_active_nodes(self):
        return chain(
            super().allowable_active_nodes(), 
            self.find_all(PassiveChain, subset=self.members_of(self.slipnet))
        )

def newg(numble: Numble, seed=8028868705202140491):
    return Numbo6Graph(numble=numble, seed=seed)


g = None

if __name__ == '__main__':
    #numble = Numble([4, 5, 6], 15)
    #numble = Numble([4, 5, 6], 34)
    numble = Numble([4, 5, 6], 1)
    g = newg(numble)
    want = g.look_for(Want)
    assert want
    #g.do_timestep(num=2)
    #booster = g.add_node(LookForOperands, behalf_of=want, activation=2.0)

    #ShowActiveNodes.start_logging()
    ShowActionList.start_logging()
    ShowActionsPerformed.start_logging()
    ShowPrimitives.start_logging()

    oot = g.look_for(OoMTagger)
    oobigt = g.look_for(OoMBigGapToWantedTagger)
    ncmm = g.look_for(NoticeCouldMakeMinus)
    difft = g.look_for(DiffTagger)
    diwt = g.look_for(DiffIsWantedTagger)
    #g.do_timestep(actor=difft, num=5)
    #g.do_timestep(actor=diwt, num=5)

    #g.do_timestep(actor=oot, num=4)
    #g.do_timestep(actor=ncmm)
    #g.do_timestep(num=39)

    g.do_timestep(actor=difft, num=4)
    #g.do_timestep(num=9)
    g.do_timestep(num=11)
    #g.do_timestep(num=26)

#    ncmp = g.as_node(g.look_for(NoticeCouldMakePlus))
#
#    pg(g, NoticeCouldMakePlus)
#
#    ShowActiveNodes.start_logging()
#    #ShowActionList.start_logging()
#    ShowActionsPerformed.start_logging()
#    ShowPrimitives.start_logging()
#    g.do_timestep(actor=NoticeCouldMakePlus)
#    pg(g, NoticeCouldMakePlus)
#    #ShowIsMatch.start_logging()
#    g.do_timestep(actor=NoticeCouldMakePlus)
#    pg(g, NoticeCouldMakePlus)
#    g.do_timestep()

#    tagger = g.look_for(OoMTagger)
#    #ShowPrimitives.start_logging()
#    ShowActionList.start_logging()
#    ShowActionsPerformed.start_logging()
#    #g.do_timestep(actor=tagger)
#    #pg(g)
#    assessor = g.neighbor(want, 'agents')
#    g.do_timestep(actor=NoticeCouldMakePlus)
#    g.do_timestep(actor=ProposeDoingNoticedOperation)
#    g.do_timestep(actor=assessor)
#    #g.print_actions()
#    #g.do_timestep(num=1)

#    # A run to completion
#    #ShowActionList.start_logging()
#    ShowActionsPerformed.start_logging()
#    #ShowPrimitives.start_logging()
#    g.do_timestep(num=57)
