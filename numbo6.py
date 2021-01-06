# numbo6.py

from NumboGraph import *
from log import *
from exc import *
from ActiveGraph import pg, pa, ps
from criteria import NotTagged

# Custom exceptions

@dataclass
class NeedOperands(FizzleAndBlock):

    agent_nodeclass = 'LookForOperands'

# Custom AcNodes

# NEXT Need to boost this node's activation much more to wake it up
class LookForOperands(Restartable, AcNode):
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
        #PrintEnv(),
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
        #PrintEnv(),
        BuildOpResult(Times, operands='nodes')
    ]

class ProposeDoingNoticedOperation(Persistent, AcNode):
    acs = [
        LookFor(
            OperatorWithAvailOperands(),
            within=InWorkspace,
            asgn_to='operator'
        ),
        AsgnNeighbors(node='operator', port_label=Quote('operands')),
        AddNode(   # TODO Fail silently if this node has already been built
            Proposal,
            action=ConsumeOperands(),
            proposed_operands='operands',
            proposed_operator='operator'
        )
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
        pdno = self.add_node(ProposeDoingNoticedOperation, member_of=self.ws)
        oot = self.add_node(OoMTagger, member_of=self.ws)
        oogtt = self.add_node(OoMGreaterThanTagger, member_of=self.ws)
        oo1bt = self.add_node(OoM1BelowWantedTagger, member_of=self.ws)

        self.add_activation_autolinks(
            (OoM1BelowWanted, ncmp),
            #(OperandBelowWanted, ncmp),
            (OoMGreaterThan, oo1bt),
            (OoM, oogtt),
            (Avail, self.look_for(NoticeSolved)),
            (Operator, pdno)  # TODO Only "noticed" Operators
        )

def newg(numble: Numble, seed=8028868705202140491):
    return Numbo6Graph(numble=numble, seed=seed)


g = None

if __name__ == '__main__':
    numble = Numble([4, 5, 6], 15)
    #numble = Numble([4, 5, 6], 34)
    g = newg(numble)
    want = g.look_for(Want)
    assert want
    #g.do_timestep(num=2)
    #booster = g.add_node(LookForOperands, behalf_of=want, activation=2.0)

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

    tagger = g.look_for(OoMTagger)
    #ShowPrimitives.start_logging()
    ShowActionList.start_logging()
    ShowActionsPerformed.start_logging()
    #g.do_timestep(actor=tagger)
    #pg(g)
    assessor = g.neighbor(want, 'agents')
    g.do_timestep(actor=NoticeCouldMakePlus)
    g.do_timestep(actor=ProposeDoingNoticedOperation)
    g.do_timestep(actor=assessor)
    #g.print_actions()
    #g.do_timestep(num=1)
