from PortGraph import PortGraph, NodeAndValue, pg, pn, Node, Tag, CouldMake
from numbonodes import *
from watcher import Watcher, Response, TagWith, TagWith2
from exc import *
from util import nice_object_repr

from itertools import product, chain, combinations
from random import sample, choice
from math import ceil


class Numble:

    def __init__(self, bricks, target):
        'bricks: a list of integers. target: an integer.'
        self.bricks = bricks
        self.target = target

    def build(self, g, container):
        '''Builds the nodes for the numble as members of the container node
        in graph g. Returns container.'''
        for brick in self.bricks:
            brick_id = g.make_node(Brick(brick))
            g.add_member_edge(container, brick_id)
            TagWith(Avail, taggee=brick_id).go(g)
        target_id = g.make_node(Target(self.target))
        g.add_member_edge(container, target_id)
        TagWith(Wanted, taggee=target_id).go(g)
        return container

    __repr__ = nice_object_repr


def prompt_for_numble():
    '''Prompts the user to enter bricks and a target at the keyboard.
    Returns a Numble object.'''
    while True:
        brick_str = input('Bricks: ')
        try:
            bricks = [int(b) for b in brick_str.split()]
            if not bricks:
                continue #TODO Probably better to throw an exception
            break
        except ValueError:
            print('Please enter the bricks as integers separated by spaces.')
            continue

    while True:
        target_str = input('Target: ')
        try:
            target = int(target_str)
            break
        except ValueError:
            print('Please enter one integer and press Enter.')
            continue

    return Numble(bricks, target)


#TODO rm
class BrickWatcher(Watcher):

    def look(self, hg):
        return [self.make_response(brick)
                    for brick in g.nodes_of_class(Brick)
                        if (not g.is_in_role(brick, 'source')
                            and
                            not g.has_tag(brick, Avail))]

    def make_response(self, brick):
        return TagWith(Avail, taggee=brick)



class WantedWatcher(Watcher):

    def look(self, hg):
        avails = hg.nodes_with_tag(Avail)
        wanteds = hg.nodes_with_tag(Wanted)
        return [self.make_response(avail, wanted)
                    for avail, wanted in product(avails, wanteds)
                        if hg.have_same_value(avail, wanted)]

    def make_response(self, avail, wanted):
        return FoundWanted(avail, wanted)


def are_close(n1, n2):
    m = max(n1, n2)
    max_dist = abs(ceil(m * 0.1))
    return abs(n1 - n2) < max_dist

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
#
#    def look(self, hg):
#        wanteds = g.nodes_with_tag(Wanted)
#        close_nodes = from_iterable(
#            g.mates_via_tag(w, Close)
#                for w in wanteds
#        )
#        for 


class FoundWanted(Response):
    def __init__(self, avail, wanted):
        self.avail = avail
        self.wanted = wanted

    def go(self, g):
        g.add_edge(self.avail, 'consumer', self.wanted, 'source')
        g.remove_tag(self.avail, Avail)
        #TODO Declare victory if wanted is Target
        if g.is_of_class(self.wanted, Target):
            raise NumboSuccess(g, self.wanted)
        #TODO Otherwise tag wanted as Avail

    __repr__ = nice_object_repr

    def annotation(self, g):
        return (
            "Hey, look! We wanted %s and we have %s."
            %
            (g.datumstr(self.wanted), g.datumstr(self.avail))
        )


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
        watchers=default_watchers,
        done=False
    )

    def __init__(self, **kwargs):
        kws = self.default_graph_attrs.copy()
        kws.update(kwargs)
        super().__init__(**kws)
        ws = self.make_node(Workspace)
        self.graph['ws'] = ws
        if 'numble' in self.graph:
            self.graph['numble'].build(self, ws)
    
    def ws(self):
        return self.graph['ws']
        
    def watchers(self):
        #TODO It would likely be a better model if the Watchers were
        #themselves Nodes in the graph.
        return self.graph['watchers']

    def do_timestep(self):
        #TODO This is very crude: it executes all Responses from all Watchers.
        #We should randomly choose just one, based on salience.
        self.graph['t'] += 1
        try:
            print('WATCHERS', self.graph['watchers'])
            responses = list(chain.from_iterable(
                watcher.look(self) for watcher in self.graph['watchers']
            ))
            if len(responses) == 0:
                responses = [Backtrack()]
            for response in responses:
                print(response.annotation(self))
                    #TODO Print only if DEBUG or LOG or something
                response.go(self)
        except FargDone as exc:
            print('\n' + exc.done_msg())
            self.graph['done'] = True

    def run(self, max_timesteps=20):
        while self.graph['t'] <= max_timesteps:
            self.do_timestep()
            if self.graph['done']:
                return
        print('''
If so great a mind as mine could not solve this numble in %s timesteps,
it must surely have no solution.
''' % max_timesteps)
            
    def expr_by_sources(self, target):
        '''Returns a string representing the expression whose ultimate
        'consumer' is target.'''
        return self.datum(target).expr_str(self, target)

    def fail(self, node):
        datum = self.datum(node)
        datum.fail(self, node)

    def cascade_fail(self, node):
        datum = self.datum(node)
        datum.cascade_fail(self, node)

        
def run(numble):
    g = NumboGraph()
    ws = g.make_node(Workspace)
    numble.build(g, ws)

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

def in_progress():
    '''This runs whatever I'm working on right now. --BEN'''
    global g
    g = NumboGraph(numble=Numble([120, 1, 2, 3, 4, 5], 121))
    pg(g)
    g.run()


if __name__ == '__main__':
    #demo()
    in_progress()

#    g = PortGraph()
#    ws = g.make_node(Workspace)
#    Numble([1, 3, 1], 3).build(g, ws)
#    pg(g)
#    rs = WantedWatcher().look(g)
#    print(rs)
#    #rs[0].go(g)  # This should tag the first Brick with Avail

