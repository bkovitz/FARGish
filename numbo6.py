# numbo6.py

from testNumboClasses import *
from log import *
from ActiveGraph import pg, pa

class NoticeCouldMake(AcNode):
    acs = [
        LookFor2(
            CTagged(Avail),
            cond=NotTheArgsOf('nodes', Quote('source'), OfClass(Plus)),
            within=InWorkspace
        ),
        #BuildOpResult(Plus, operands='nodes')
    ]

class NumboGraph(Graph):
    def __init__(self, numble, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.nodeclasses.update(nodeclasses)
        self.add_nodeclasses(
            AllBricksAvail, NoticeAllBricksAreAvail, FillParamScout, Proposal
        )
        self.port_mates += port_mates

        # Make initial nodes
        self.add_node(Workspace)
        numble.build(self, self.ws)
        self.add_node(NoticeSolved, member_of=self.ws, within=self.ws)
        self.add_node(NoticeSum, member_of=self.ws)

    def consume_operands(
        self,
        operand_ids: NRefs,
        operator_class: CRef,
        actor=None
    ):
        if not self.has_tag(operand_ids, Avail):
            return  # TODO Raise a failure exception?
        operator_class = self.as_nodeclass(operator_class)
        operator_id = self.add_node(
            operator_class, 
            operands=operand_ids,
            member_of=self.containers_of(actor)
        )
        result_id = self.add_node(
            Block,
            value=arith_result0(self, operator_class, operand_ids),
            source=operator_id,
        )
        self.move_tag(Avail, operand_ids, result_id)
        self.add_tag(Consumed, operand_ids)
        self.add_tag(Done, actor)

def newg(numble: Numble, seed=8028868705202140491):
    return NumboGraph(numble=numble, seed=seed)


g = None

if __name__ == '__main__':
    ShowActionList.start_logging()
    ShowActionsPerformed.start_logging()
    g = newg(Numble([4, 5, 6], 15))
    pg(g)
