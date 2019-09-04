# numbo2.py -- Run this to run Numbo

from numble import Numble, prompt_for_numble
from PortGraph import PortGraph, NodeAndValue, pg, pn, Node, Tag, CouldMake
from numbonodes import *
from watcher import Watcher, Response, TagWith, TagWith2
from exc import *
from util import nice_object_repr, reseed
from log import ShowReponseList
from submatch import bdxs_for_datums

from itertools import product, chain, combinations
from random import sample, choice, choices
from math import ceil


class WantedWatcher(Watcher):

    def look(self, hg):
        avails = hg.nodes_with_tag(Avail)
        wanteds = hg.nodes_with_tag(Wanted)
        return [self.make_response(avail, wanted)
                    for avail, wanted in product(avails, wanteds)
                        if (avail != wanted
                            and
                            hg.have_same_value(avail, wanted)
                        )
        ]

    def make_response(self, avail, wanted):
        return FoundWanted(avail, wanted)

class FoundWanted(Response):
    def __init__(self, avail, wanted):
        self.avail = avail
        self.wanted = wanted

    def go(self, g):
        g.add_edge(self.avail, 'consumer', self.wanted, 'source')
        g.remove_tag(self.avail, Avail)
        if g.is_of_class(self.wanted, Target):
            raise NumboSuccess(g, self.wanted)
        else:
            g.add_tag(Avail, self.wanted)
            #TODO Somehow remember the class of the Wanted tag
            g.replace_tag(self.wanted, Wanted, WasWanted)

    __repr__ = nice_object_repr

    def annotation(self, g):
        return (
            "Hey, look! We wanted %s and we have %s."
            %
            (g.datumstr(self.wanted), g.datumstr(self.avail))
        )


def are_close(n1, n2):
    m = max(n1, n2)
    max_dist = abs(ceil(m * 0.1))
    return n1 != n2 and abs(n1 - n2) < max_dist

class CloseNumbersTagger(Watcher):

    def look(self, hg):
        nodes = hg.nodes_of_class(Number)
        nodes = list(hg.nodes_without_tag(CloseNumbers, nodes=nodes))
        nvs = [NodeAndValue(node, hg.value_of(node)) for node in nodes]
        nv_pairs = list(combinations(nvs, 2))
#        close_node_pairs = [
#            (nv1.node, nv2.node)
#                for nv1, nv2 in nv_pairs
#                    if are_close(nv1.value, nv2.value)
#        ]
#        return [
#            TagWith2(CloseNumbers, [node1, node2])
#                for node1, node2 in close_node_pairs
#        ]
        return [
            TagWith2(CloseNumbers, [nv1.node, nv2.node])
                for nv1, nv2 in nv_pairs
                    if are_close(nv1.value, nv2.value)
        ]

#class CloseToWantedWatcher(Watcher):
#    '''Looks for Wanted nodes tagged Close to another Number. Starts a
#    Watcher to find or build the difference.'''
#
#    def look(self, hg):
#        wanteds = hg.nodes_with_tag(Wanted)
#        close_mates = [
#            (w, hg.mates_via_tag(w, Close))
#                for w in wanteds
#        ]
#        for w, mates in close_mates:
#            for mate in mates:
#                hg.start_watcher(



class AvailWatcher(Watcher):

    def look(self, hg):
        avails = list(hg.nodes_with_tag(Avail))
        #TODO Very crude for now: just pick two available numbers and
        # combine them with a random operator
        if len(avails) >= 2:
            avails = sample(avails, 2)
            try:
                return [CombineOperands(hg, avails)]
            except FargCantRespond:
                return []
        else:
            return []

class CombineOperands(Response):

    def __init__(self, g, operands):
        self.g = g
        self.operands = operands
        possible_operators = [
            op for op in all_operators
                if not self.failed_with_these_operands(op)
        ]
        if possible_operators:
            self.operator = choice(possible_operators)()
        else:
            raise FargCantRespond()

    def failed_with_these_operands(self, operator):
        former_consumerss = []
        for operand in self.operands:
            former_consumerss.append(
                set(neighbor for neighbor in self.g.neighbors(
                                 operand, 'former_consumer'
                             )
                             if self.g.is_of_class(neighbor, operator)
                )
            )
        return len(set.intersection(*former_consumerss)) > 0


    #TODO Stop passing g to .go()
    def go(self, g):
        operator_id = g.make_node(self.operator)
        for operand in self.operands:
            g.add_edge(operand, 'consumer', operator_id, 'source')
            g.remove_tag(operand, Avail)
        result_value = self.operator.result_value(g, operator_id)
        result_id = g.make_node(Block(result_value))
        print('Made %s.' % g.datumstr(result_id)) #TODO Move this
        g.add_edge(operator_id, 'consumer', result_id, 'source')
        TagWith(Avail, taggee=result_id).go(g)

    def annotation(self, g):
        return "Let's try %s on %s." % (
            self.operator, ', '.join(g.datumstr(o) for o in self.operands)
        )


class EquationResultWatcher(Watcher):

    def __init__(self, operands, operator, result):
        'EquationResultWatcher([2, 3], Times, 6)'
        self.operands = [Number(o) for o in operands]
        self.operator = operator
        self.result = Number(result)

    def look(self, hg):
        result_nodes = list(
            node for node in hg.nodes_matching_datum(self.result)
                     if (hg.has_tag(node, Wanted)
                         and
                         not hg.is_in_role(node, 'consumer')
                     )
        )
        print('RESULT_NODES', self.result, result_nodes)
        responses = []
        for result_node in result_nodes:
            for d in bdxs_for_datums(self.operands, hg):
                for r in self.make_responses(hg, d, result_node):
                    responses.append(r)
        #Somewhat HACKish: if there are any opportunities to complete
        #an equation, we don't return any of MakeSeek responses even if
        #there were any, since those will likely set up a subgoal that
        #has already been achieved.
        fills = [r for r in responses if isinstance(r, FillInOperator)]
        if fills:
            return fills
        else:
            return responses

    def make_responses(self, hg, bindings_dict, result_node):
        if len(bindings_dict) == len(self.operands): # every operand has a mate
            if all(hg.has_tag(bindings_dict[o], Avail) for o in self.operands):
                return [
                    FillInOperator(bindings_dict, self.operator, result_node)
                ]
            else:
                # Start WantedWatchers for the non-Avail operators
                return [
                    MakeSeek(o.value)
                        for o in self.operands
                            if (not hg.has_tag(
                                        bindings_dict.get(o, None),
                                        Avail)
                                and
                                not hg.already_seeking(Number(o.value))
                            )
                ]
        return [] #TODO STUB

    __repr__ = nice_object_repr

class FillInOperator(Response):

    def __init__(self, bindings_dict, operator_datum, result_node):
        self.bindings_dict = bindings_dict
        self.operator_datum = operator_datum
        self.result_node = result_node

    def go(self, g):
        #TODO OAOO with CombineOperands; should have a consume() function
        operator_id = g.make_node(self.operator_datum)
        for operand in self.bindings_dict.values():
            g.add_edge(operand, 'consumer', operator_id, 'source')
            g.remove_tag(operand, Avail)
        g.add_edge(operator_id, 'consumer', self.result_node, 'source')
        g.remove_tag(self.result_node, Wanted)
        TagWith(Avail, taggee=self.result_node).go(g)

class MakeSeek(Response):

    def __init__(self, wanted_value):
        self.wanted_value = wanted_value

    def go(self, g):
        node = g.make_node(Block(self.wanted_value))
        g.add_tag(Seek, node)


class Backtrack(Response):
    '''Pretty crude: just undoes all existing Avail tags, tags any operators
    that produced the the Avails' taggees with Failed, and retags the
    operands with Avail. Actually, all the decision-making is pushed to
    the nodes tagged Avail.'''

    def go(self, g):
        for node in list(g.nodes_with_tag(Avail)):
            g.fail(node)

#        avails = g.nodes_with_tag(Avail)
#        for avail in avails:
#            taggee = g.taggee_of(avail)
#            taggee_datum = g.datum(taggee_id)
#            taggee_datum.fail(g, taggee_id)
#            g.remove_node(g, avail)


default_watchers = [WantedWatcher(), AvailWatcher(), CloseNumbersTagger()]

class NumboGraph(PortGraph):

    default_graph_attrs = dict(
        t=0,
        done=False,
        num_timesteps=20,
        seed=None
    )

    def __init__(self, **kwargs):
        kws = self.default_graph_attrs.copy()
        kws.update(kwargs)
        if kws.get('num_timesteps', None) is None:
            kws['num_timesteps'] = self.default_graph_attrs['num_timesteps']
        kws['seed'] = reseed(kws.get('seed', None))
        super().__init__(**kws)
        ws = self.make_node(Workspace)
        self.graph['ws'] = ws
        if 'numble' in self.graph:
            self.graph['numble'].build(self, ws)
    
    def __repr__(self):
        return 'NumboGraph'

    def ws(self):
        return self.graph['ws']
        
    def done(self):
        return self.graph['done']

    def is_fully_sourced(self, node):
        if not self.has_node(node):
            return False
        datum = self.datum(node)
        if datum.needs_source:
            sources = self.neighbors(node, port_label='source')
            if sources:
                return all(self.is_fully_sourced(source) for source in sources)
            else:
                return False
        else:
            return True

    def watchers(self):
        return self.nodes_of_class(Watcher)

    def set_done(self, done):
        self.graph['done'] = done

    def do_timestep(self):
        self.graph['t'] += 1
        print('t=%s' % self.graph['t']) #TODO Set a global flag for this
        self.decay_saliences()
        responses = list(chain.from_iterable(
            self.datum(watcher).look(self, watcher)
                for watcher in self.watchers()
        ))
        if ShowReponseList.is_logging():
            print('responses=%s' % (responses,))
        if len(responses) == 0:
            responses = [Backtrack()]
        #for response in responses:
        #response = choice(responses)
        response = choices(
            responses, weights=[r.salience for r in responses], k=1
        )[0]
        print(response)
        #print(response.annotation(self))
            #TODO Print only if DEBUG or LOG or something
        response.go(self)

    def run(self, num_timesteps=None, show_fail=False):
        if num_timesteps is None:
            num_timesteps = self.graph['num_timesteps']
        for i in range(num_timesteps):
            self.do_timestep()
            if self.graph['done']:
                print(self.graph['done'])
                return
        else:
            if show_fail:
                print('''
If so great a mind as mine could not solve this numble in %s timesteps,
it must surely have no solution.
''' % self.graph['t'])
            
#    def expr_by_sources(self, target):
#        '''Returns a string representing the expression whose ultimate
#        'consumer' is target.'''
#        return self.datum(target).expr_str(self, target)

    def expr_as_equation(self, target):
        '''Returns an expr.Equation representing the expression whose ultimate
        'consumer' is target.'''
        source = self.neighbor(target, port_label='source')
        #TODO What if there's more than one source? Or none?
        return expr.Equation(self.expr(source), self.expr(target))

    def expr(self, node):
        return self.datum(node).expr(self, node)

    def blocks_str(self):
        result = []
        for block in self.nodes_of_class(Block):
            if self.has_source(block) and not self.has_consumer(block):
                result.append(
                    str(self.expr(block)) + ' = ' + str(self.value_of(block))
                )
        return '\n'.join(result)

    def has_source(self, node):
        return self.neighbor(node, port_label='source')

    def has_consumer(self, node):
        return self.neighbor(node, port_label='consumer')

    def already_seeking(self, datum):
        try:
            next(self.nodes_matching_datum(datum, self.nodes_with_tag(Seek)))
            return True
        except StopIteration:
            return False

    def fail(self, node):
        datum = self.datum(node)
        datum.fail(self, node)

    def cascade_fail(self, node):
        datum = self.datum(node)
        datum.cascade_fail(self, node)


def pb(g):
    'Print the Blocks built from Bricks and their expressions'
    print(g.blocks_str())

        
#TODO Make this into a proper unit test
def testInstantSuccess():
    g = NumboGraph(watchers=[WantedWatcher()], numble=Numble([1, 3, 1], 3))
    pg(g)
    g.do_timestep()
    print()
    pg(g)

def testSimple():
    #Sometimes this backtracks
    global g
    g = NumboGraph(numble=Numble([1, 1, 1], 3))
    pg(g)
    g.do_timestep()
    print()
    pg(g)

def demo():
    '''Run this for Doug.'''
    global g
    while True:
        numble = prompt_for_numble()
        g = NumboGraph(numble=numble)
        g.run()

def run(numble, seed=None, num_timesteps=None):
    global g
    g = NumboGraph(seed=seed)
    numble.build(g, g.ws())
    g.make_node(OperandsCouldMakeTagger)
    g.run(num_timesteps=num_timesteps)
    return g

def close():
    global g
    g = NumboGraph(numble=Numble([120, 1, 2, 3, 4, 5], 121))
    pg(g)
    g.run()

def simplest(**kwargs):
    run(Numble([1, 1], 2), **kwargs)

def in_progress(**kwargs):
    '''This runs whatever I'm working on right now. --BEN'''
    simplest(**kwargs)


def go(seed=6185774907678598918, num_timesteps=20):
    #ShowReponseList.start_logging()
    global g
    in_progress(seed=seed, num_timesteps=num_timesteps)
    if not g.done():
        pb(g)


if __name__ == '__main__':
    #demo()
    go()
    #in_progress()
    print('SEED', g.graph['seed'])

#    g = PortGraph()
#    ws = g.make_node(Workspace)
#    Numble([1, 3, 1], 3).build(g, ws)
#    pg(g)
#    rs = WantedWatcher().look(g)
#    print(rs)
#    #rs[0].go(g)  # This should tag the first Brick with Avail


