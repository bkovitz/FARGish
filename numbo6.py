# numbo6.py

from pprint import pprint as pp

from NumboGraph import *
from log import *
from exc import *
from ActiveGraph import pg, pa, pai, ps
from criteria import NotTagged, TupAnd as CTupAnd

# Custom exceptions

@dataclass
class NeedOperands(FizzleAndBlock):

    agent_nodeclass = 'LookForOperands'

# Custom AcNodes

class LookForOperands(OneShot, AcNode):
    # TODO If no operands, raise an alarm
    acs = [
        All(CTagged(Avail), within=MyContext),
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
                within=InWorkspace,
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
                within=InWorkspace,
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
                within=InWorkspace,
            ),
            NeedOperands.from_env()
        ),
        DeTup(asgn_to=('minuend', 'subtrahend')),
        BuildOpResult(Minus, minuend='minuend', subtrahend='subtrahend')
    ]

class ProposeDoingNoticedOperation(Persistent, AcNode):
    acs = [
        LookFor(
            OperatorWithAvailOperands(),
            within=InWorkspace,
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
            proposed_operator='operator'
        ),
        Boost(nodes='node')
    ]

class Numbo6Graph(NumboGraph):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.max_actions = 2
        self.add_nodeclasses(LookForOperands)

    def make_initial_nodes(self):
        super().make_initial_nodes()
        target = self.look_for(Target, within=self.ws)
        want = self.tag_of(target, Want)
        assessor = self.add_node(AssessProposal, behalf_of=want)
        self.set_support_from_to(want, assessor, 1.0)
        ncmp = self.add_node(NoticeCouldMakePlus, member_of=self.ws)
        ncmt = self.add_node(NoticeCouldMakeTimes, member_of=self.ws)
        ncmm = self.add_node(NoticeCouldMakeMinus, member_of=self.ws)
        pdno = self.add_node(ProposeDoingNoticedOperation, member_of=self.ws)
        difft = self.add_node(DiffTagger, member_of=self.ws)
        diwt = self.add_node(DiffIsWantedTagger, member_of=self.ws)
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
            (DiffIsWanted, ncmm),
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
    g.do_timestep(num=39)

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
