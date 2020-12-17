# numbo6.py

from testNumboClasses import *
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
        self.add_node(NoticeCouldMakePlus, member_of=self.ws)

    #OAOO NumboTestGraph
    def build_op_and_result(self, operator_class: CRef, actor=None, **kwargs) \
    -> Tuple[NRef, NRef]:
        '''Builds operator node, linked via port_labels to existing nodes
        as provided in kwargs. Builds result Block with value calculated
        by operator_class.result_value(). Returns the new nodes in a tuple:
        (operator, result).'''
        # TODO Raise an exception if missing or improper operands?
        if 'member_of' not in kwargs:
            kwargs['member_of'] = self.containers_of(actor)
        operator = self.add_node(operator_class, builder=actor, **kwargs)
        result_value = self.call_method(operator, 'result_value')
        result = self.add_node(
            Block, value=result_value, source=operator, builder=actor
        )
        return (operator, result)

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
