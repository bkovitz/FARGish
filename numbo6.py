# numbo6.py

from NumboGraph import *
from log import *
from exc import *
from ActiveGraph import pg, pa

# Custom exceptions

@dataclass
class NeedOperands(FizzleAndBlock):

    agent_nodeclass = 'LookForOperands'

# Custom AcNodes

class LookForOperands(AcNode):
    # TODO If no operands, raise an alarm
    acs = [
        All(CTagged(Avail), within=MyContext),
        Boost()
    ]

class NoticeCouldMakePlus(Persistent, AcNode):
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
        PrintEnv(),
        BuildOpResult(Plus, operands='nodes')
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
        self.add_nodeclasses(LookForOperands)

    def make_initial_nodes(self):
        super().make_initial_nodes()
        self.add_node(NoticeCouldMakePlus, member_of=self.ws)
        self.add_node(ProposeDoingNoticedOperation, member_of=self.ws)


def newg(numble: Numble, seed=8028868705202140491):
    return Numbo6Graph(numble=numble, seed=seed)


g = None

if __name__ == '__main__':
    g = newg(Numble([4, 5, 6], 15))
    want = g.look_for(Want)
    assert want
    #g.do_timestep(num=2)
    #booster = g.add_node(LookForOperands, behalf_of=want, activation=2.0)

    pg(g, NoticeCouldMakePlus)

    ShowActionList.start_logging()
    ShowActionsPerformed.start_logging()
    ShowPrimitives.start_logging()
    g.do_timestep(actor=NoticeCouldMakePlus)
    pg(g, NoticeCouldMakePlus)
    ShowIsMatch.start_logging()
    g.do_timestep(actor=NoticeCouldMakePlus)
    pg(g, NoticeCouldMakePlus)
