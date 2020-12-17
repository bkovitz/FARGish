# numbo6.py

from NumboGraph import *
from log import *
from ActiveGraph import pg, pa

class NoticeCouldMakePlus(AcNode):
    acs = [
        LookForTup(
            [CTagged(Avail), CTagged(Avail)],
            tupcond=NotTheArgsOf(Plus, 'source'),
            within=InWorkspace
        ),
        BuildOpResult(Plus, operands='nodes')
    ]

class N6Graph(NumboGraph):

    def make_initial_nodes(self):
        super().make_initial_nodes()
        self.add_node(NoticeCouldMakePlus, member_of=self.ws)


def newg(numble: Numble, seed=8028868705202140491):
    return N6Graph(numble=numble, seed=seed)


g = None

if __name__ == '__main__':
    ShowActionList.start_logging()
    ShowActionsPerformed.start_logging()
    ShowPrimitives.start_logging()
    #pg(g)
    g = newg(Numble([4, 5, 6], 15))
    g.do_timestep(num=2)
