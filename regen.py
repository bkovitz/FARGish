from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar
from dataclasses import dataclass

from StdGraph import Graph
from ActiveGraph import pg, pa
from Node import Node, NRef, NRefs, MaybeNRef, PortLabel, PortLabels, CRef, \
    as_classname
from Action import Action, Actions, Build, Raise, SelfDestruct, FuncAction, \
    ResetAndKeepTrying
from ActiveNode import ActiveNode, ActionNode, make_action_sequence, Start, \
    Completed, ActionSeqNode
from criteria import Tagged as CTagged, NotTagged, HasValue, OfClass, \
    NotTaggedTogetherWith, HasAttr, NotNode, Criterion, Activated, \
    PossibleMates
from util import as_iter, as_set

from numbo5 import SeekNode, BuildProposal, SeekAndGlom  # HACK


def make_regenerator(
    g,
    seed: NRef,
    place_in: NRef,
    relevant_port_labels: PortLabels
):
    '''Build an ActionSeqNode and fill it with ActionNodes that regenerate
    the seed.'''
    seedid = g.as_nodeid(seed)
    result = g.add_node(ActionSeqNode, member_of=place_in)
    exclude: Set[NodeId] = set([seedid])
    to_do = [seedid]
    while to_do:
        nodeid = to_do.pop(0)
        for port_label in relevant_port_labels:
            neighbors = g.neighbors(nodeid, port_label) - exclude
            to_do += neighbors
            exclude.update(neighbors)
                
            print(g.nodestr(nodeid), repr(port_label), neighbors)
            if not neighbors:
                continue
            elif port_label == 'source':
                for n in neighbors:
                    g.add_next_member(
                        result, SeekNode, 
                    )
                    g.add_next_member(
                        result, BuildProposal
                    )
            else:
                g.add_next_member(
                    result, SeekAndGlom, criteria=PossibleMates(port_label)
                )
                
    return result
