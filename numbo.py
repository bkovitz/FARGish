# numbo2.py -- Run this to run Numbo

from numble import Numble, prompt_for_numble
from PortGraph import PortGraph, NodeAndValue, pg, pn, ps, Node, Tag, CouldMake
from numbonodes import *
from watcher import Watcher, Response, TagWith, TagWith2
from exc import *
from util import nice_object_repr, reseed, sample_without_replacement
from log import ShowResponseList, ShowResponseResults
from submatch import bdxs_for_datums
import support

from itertools import product, chain, combinations
from random import sample, choice, choices
from operator import attrgetter
from math import ceil


class NumboGraph(PortGraph):

    default_graph_attrs = dict(
        t=0,
        done=False,
        num_timesteps=40,
        seed=None,
        running=False
    )

    def __init__(self, **kwargs):
        kws = self.default_graph_attrs.copy()
        kws.update(kwargs)
        if kws.get('num_timesteps', None) is None:
            kws['num_timesteps'] = self.default_graph_attrs['num_timesteps']
        kws['seed'] = reseed(kws.get('seed', None))
        super().__init__(**kws)
        self.consecutive_timesteps_with_no_response = 0
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
        for i in range(1):
            support.propagate(self, max_total_support=300)
        support.log_support(g)
#        responses = list(chain.from_iterable(
#            self.datum(watcher).look(self, watcher)
#                for watcher in self.watchers()
#        ))
        responses = []
        for watcher in self.watchers():
            for response in self.datum(watcher).look(self, watcher):
                if response is not None:
                    #HACK: Overriding the Response object's salience
                    response.salience = max(
                        self.support_for(watcher),
                        response.salience
                    )
                    responses.append(response)
        if ShowResponseList.is_logging():
            print('Responses generated:')
            print('RESP', responses)
            for response in sorted(responses, key=attrgetter('salience')):
                print('  %.3f (%.3f) %s' % (
                    response.salience,
                    response.action_threshold,
                    response.gstr(self)
                ))
        responses = [r for r in responses if r.salience >= r.action_threshold]
        if len(responses) == 0:  #TODO Better criterion for backtracking
            #responses = [Backtrack()]
            self.consecutive_timesteps_with_no_response += 1
            if self.consecutive_timesteps_with_no_response >= 60:
                self.set_done(TooManyTimestepsWithNoResponse(
                    self.consecutive_timesteps_with_no_response
                ))
                if ShowResponseResults.is_logging():
                    print(self.done())
        else:
            self.consecutive_timesteps_with_no_response = 0
        #for response in responses:
        #response = choice(responses)
#        response = choices(
#            responses, weights=[r.salience for r in responses], k=1
#        )[0]

        # TODO global parameter: k  (number of responses to do each timestep)
        for response in sample_without_replacement(
            responses, k=2, weights=[r.salience for r in responses]
        ):
            #print(response)
            response.go(self)
            if ShowResponseResults.is_logging():
                ann = response.annotation(self)
                #print('IS', ann.__class__, isinstance(ann, FargDone))
                #if not isinstance(ann, FargDone) or not self['running']:
                print(ann)
            if isinstance(response, Decision):
                break
        self.do_touches()

    def run(self, num_timesteps=None, show_fail=False):
        try:
            self.graph['running'] = True
            if num_timesteps is None:
                num_timesteps = self.graph['num_timesteps']
            for i in range(num_timesteps):
                self.do_timestep()
                if self.graph['done']:
                    #if not ShowResponseResults.is_logging():
                    #print(self.graph['done'])
                    return
            else:
                if show_fail:
                    print('''
If so great a mind as mine could not solve this numble in %s timesteps,
it must surely have no solution.
''' % self.graph['t'])
        finally:
            self.graph['running'] = False
            
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
        if node is None:
            return expr.UnspecifiedExpr()
        else:
            #print('NODE', node, self.datum(node))
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
#def testInstantSuccess():
#    g = NumboGraph(watchers=[WantedWatcher()], numble=Numble([1, 3, 1], 3))
#    pg(g)
#    g.do_timestep()
#    print()
#    pg(g)

def testSimple():
    #Sometimes this backtracks
    global g
    g = NumboGraph(numble=Numble([1, 1, 1], 3))
    pg(g)
    g.do_timestep()
    print()
    pg(g)

def demo(num_timesteps=200):
    '''Run this for Doug.'''
    global g
    ShowResponseResults.start_logging()
    while True:
        numble = prompt_for_numble()
        if numble is None:
            break
        run(numble=numble, num_timesteps=num_timesteps)
        #g = NumboGraph(numble=numble)
        #g.run()

def run(numble=None, seed=None, num_timesteps=None):
    global g
    g = NumboGraph(seed=seed)
    if numble is None:
        numble = prompt_for_numble()
    numble.build(g, g.ws())
    #g.make_node(CouldMakeFromOperandsTagger)
    #g.make_node(BottomUpOperandFinder)
    g.make_node(BottomUpOperandScout)
    g.make_node(SameNumberScout)
    g.make_node(CloseNumbersScout)
    g.run(num_timesteps=num_timesteps)
    return g

def close(**kwargs):
    run(Numble([120, 1, 2, 3, 4, 5], 121), **kwargs)

def no_arithmetic(**kwargs):
    run(Numble([2, 3, 4], 3), **kwargs)

def simplest(**kwargs):
    run(Numble([1, 1], 2), **kwargs)

def six(**kwargs):
    run(Numble([1, 1, 1, 1, 1], 6), **kwargs)

def slog7(**kwargs):
    run(Numble([2, 3, 4, 5, 6, 7], 8), **kwargs)

#def in_progress(seed=5680298187468365268, **kwargs):
def in_progress(seed=4611039348018989335, num_timesteps=1, **kwargs):
    '''This runs whatever I'm working on right now. --BEN'''
    #simplest(**kwargs)
    ShowResponseResults.start_logging()
    ShowResponseList.start_logging()
    #ShowOperandCandidates.start_logging()
    #run(Numble([2, 3, 5], 10), seed=seed, **kwargs)
    slog7(seed=seed, num_timesteps=num_timesteps, **kwargs)
    #close(seed=seed, num_timesteps=num_timesteps, **kwargs)


def go(seed=6185774907678598918, num_timesteps=10):
    global g
    ShowResponseList.start_logging()
    ShowResponseResults.start_logging()
    in_progress(seed=seed, num_timesteps=num_timesteps)
    print('SEED', g.graph['seed'])
    if not g.done():
        pb(g)


if __name__ == '__main__':
    #demo()
    #go()
    in_progress()

#    g = PortGraph()
#    ws = g.make_node(Workspace)
#    Numble([1, 3, 1], 3).build(g, ws)
#    pg(g)
#    rs = WantedWatcher().look(g)
#    print(rs)
#    #rs[0].go(g)  # This should tag the first Brick with Avail


