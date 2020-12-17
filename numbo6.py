# numbo6.py

from NumboGraph import *
from log import *
from ActiveGraph import pg, pa


class NoticeCouldMakePlus(Persistent, AcNode):
    acs = [
        LookForTup(
            [CTagged(Avail), CTagged(Avail)],
            tupcond=NotTheArgsOf(Plus, 'source'),
            within=InWorkspace
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
        PrintEnv(),
        AddNode(   # TODO Fail silently if this node has already been built
            Proposal,
            action=ConsumeOperands(),
            proposed_operands='operands',
            proposed_operator='operator'
        )
    ]

class Numbo6Graph(NumboGraph):

    def make_initial_nodes(self):
        super().make_initial_nodes()
        self.add_node(NoticeCouldMakePlus, member_of=self.ws)
        self.add_node(ProposeDoingNoticedOperation, member_of=self.ws)


def newg(numble: Numble, seed=8028868705202140491):
    return Numbo6Graph(numble=numble, seed=seed)


g = None

if __name__ == '__main__':
    ShowActionList.start_logging()
    ShowActionsPerformed.start_logging()
    ShowPrimitives.start_logging()
    #pg(g)
    g = newg(Numble([4, 5, 6], 15))
    #g.do_timestep(num=2)
