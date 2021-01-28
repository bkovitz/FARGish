# numbo6.py

from pprint import pprint as pp
from itertools import chain

from NumboGraph import *
from log import *
from exc import *
from ActiveGraph import pg, pa, pai, paa, ps
from ActiveNode import ActionNode
from PassiveChain import PassiveChain, make_passive_chain
from criteria import NotTagged, TupAnd as CTupAnd

# Custom exceptions

# Custom AcNodes

# Custom Actions

    # HACK
    # TODO Override this in RunPassiveChain somehow
#    anc_dict = {
#        ('Diff', 'DiffIsWanted'): DiffIsWantedTagger,
#        ('DiffIsWanted', 'Minus'): NoticeCouldMakeMinus,
#        ('Minus', 'Proposal'): ProposeDoingNoticedOperation
#    }

class Numbo6Graph(NumboGraph):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.max_actions = 2

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
        make_passive_chain(
            self, Diff(value=1), DiffIsWanted, Minus, Proposal,
            member_of=sl
        )
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
    ShowIsMatch.start_logging()

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
    g.do_timestep(num=38)
    print(g.done())

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
