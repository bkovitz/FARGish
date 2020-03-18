# testCodegen.py -- Tests of the FARGish grammar and compilation to Python

import unittest
from pprint import pprint as pp

from codegen import make_python, compile_fargish
from PortGraph import PortGraph, Node, pg
from TimeStepper import TimeStepper
from LinkSpec import LinkSpec
from log import ShowActiveNodes, ShowActionList, ShowActionsChosen, ShowResults

class TestGraph(TimeStepper, PortGraph):
    pass

class TestCodegen(unittest.TestCase):

    def test_one_simple_node(self):
        g = PortGraph()
        prog = '''
SomeNode
'''
        exec(compile_fargish(prog), globals())
        # The globals() argument makes exec put the newly defined classes
        # into our globals. Python does not provide a way (that works) for
        # exec to add classes to our local variables. This is unfortunate,
        # because each unit test leaves its class definitions in the global
        # space, potentially influencing unit tests that run later.
        g.make_node(SomeNode)
        got = g.all_datums()
        expect = [SomeNode()]
        self.assertCountEqual(got, expect)

    def test_node_with_arg(self):
        g = PortGraph()
        prog = '''
Number(n)
Brick : Number'''
        #make_python(prog) #DEBUG
        exec(compile_fargish(prog), globals())
        b = Brick(2)
        g.make_node(Brick(2))
        got = g.all_datums()
        expect = [Brick(n=2)]
        self.assertCountEqual(got, expect)

    def test_autolink(self):
        #TODO Generate code for node_params, not auto_links
        prog = '''
target -- tags
Number(n)
Scout(target)'''
        #make_python(prog) #DEBUG
        exec(compile_fargish(prog), globals())
        g = PortGraph(port_mates=port_mates)
        #nid = g.make_node(Number(3))
        nid = g.make_node(Number, 3)
        #sid = g.make_node(Scout(nid))
        sid = g.make_node(Scout, nid)
        self.assertTrue(g.has_hop(sid, 'target', nid, 'tags'))

    def test_build_agent(self):
        prog = '''
Client
  agent: Agent

Agent
'''
        #make_python(prog, debug=1) #DEBUG
        exec(compile_fargish(prog), globals())
        #ShowActionList.start_logging()
        #ShowActionsChosen.start_logging()
        g = TestGraph(port_mates=port_mates)
        client = g.make_node(Client)
        g.do_timestep()
        self.assertEqual(len(g), 2)
        agent = g.neighbor(client, port_label='agents')
        self.assertEqual(g.class_of(agent), Agent)
        self.assertTrue(g.has_hop(agent, 'behalf_of', client, 'agents'))

        # Once the agent is built, the client should not build another.
        g.do_timestep(num=5)
        self.assertEqual(len(g), 2)

        # Let's build another Client. It should get its own Agent.
        client2 = g.make_node(Client)
        g.do_timestep()
        self.assertEqual(len(g), 4)
        agent2 = g.neighbor(client2, port_label='agents')
        self.assertEqual(g.class_of(agent2), Agent)
        self.assertNotEqual(agent, agent2)
        g.do_timestep(num=5)
        self.assertEqual(len(g), 4)

        #TODO There needs to be a unit test that fully exercises the
        # ability to pass a Node ctor a port_label argument for which it
        # has not NodeParam, thus forcing it to make a link not explicitly
        # provided for in the Node's parameters. As of 13-Mar-2013, the
        # PortGraph.exactly_matches_kwargs() function does not check for
        # such links. That should cause PortGraph.already_built() to return
        # a spurious result.

    #TODO unteest
    def teest_see_do(self):
        g = TestGraph()
        prog = '''
tags -- taggees

Tag
Found, Done, SomethingElse : Tag

Target
Simplest
  => do_something()

Builder
  => build ConsumeOperands(1, 2, 3)

Scout
  see t := NodeOfClass(Target)
  => add_tag(Found, t)

Scout2
  see Not(Tagged(Done, this))
  => do_something(this)

Scout3
  see t1 := NodeOfClass(Target)
  => add_tag(Found, t1)
  else t2 := NodeOfClass(Found)
  => add_tag(Done, t2)

Scout4
  see t1 := NodeOfClass(Target)
  => add_tag(Found, t1)
  else t2 := NodeOfClass(Found)
  => add_tag(Done, t2)
  else t3 := NodeOfClass(SomethingElse)
  => do_something(Found, t2, t3)
'''
        make_python(prog) #DEBUG

    #TODO undo teest
    def teest_see_do1(self):
        g = TestGraph()
        prog = '''
Scout
  => do_something()
'''
        make_python(prog, debug=True) #DEBUG

        '''
    _result = []
    _result.append(do_something())
    return _result
'''

    #TODO undo teest
    def teest_see_do2(self):
        g = TestGraph()
        prog = '''
Scout
  => predefined_action
'''
        make_python(prog, debug=True) #DEBUG
        '''
    _result = []
    _result.append(predefined_action)
    return _result
'''

    #TODO undo teest (or maybe not)
    def teest_see_do3(self):
        g = TestGraph()
        prog = '''
Scout
  => 111  # This is actually crazy and FARGish shouldn't allow it
'''
        make_python(prog, debug=True) #DEBUG
        '''
    _result = []
    _result.append(111)
    return _result
'''

    #TODO undo teest
    def teest_see_do4(self):
        g = TestGraph()
        prog = '''
goal -- tags

SuccessScout(goal)

Scout
    => build SuccessScout(goal=this)
'''
        make_python(prog, debug=True) #DEBUG
        '''
        _result = []
        _kwargs = {'goal', _thisid}
        if not _g.already_built(SuccessScout, kwargs=_kwargs):
            _result.append(Build(SuccessScout, kwargs=_kwargs))
        return _result
'''
