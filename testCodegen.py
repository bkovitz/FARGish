# testCodegen.py -- Tests of the FARGish grammar and compilation to Python

import unittest
from pprint import pprint as pp

from codegen import make_python, compile_fargish
from PortGraph import PortGraph, Node, pg
from TimeStepper import TimeStepper
from LinkSpec import LinkSpec
from NodeSpec import BuildSpec
from log import ShowActiveNodes, ShowActionList, ShowActionsChosen, ShowResults

class TestGraph(TimeStepper, PortGraph):
    pass

class TestCodegen(unittest.TestCase):

    def testOneSimpleNode(self):
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

    def testNodeWithArg(self):
        g = PortGraph()
        prog = '''
Number(n)
Brick : Number'''
        exec(compile_fargish(prog), globals())
        g.make_node(Brick(2))
        got = g.all_datums()
        expect = [Brick(n=2)]
        self.assertCountEqual(got, expect)

    def testAutoLink(self):
        g = PortGraph()
        prog = '''
target -- tags
Number(n)
Scout(target)'''
        #make_python(prog) #DEBUG
        exec(compile_fargish(prog), globals())
        nid = g.make_node(Number(3))
        sid = g.make_node(Scout(nid))
        self.assertTrue(g.has_hop(sid, 'target', nid, 'tags'))

    def testBuildAgent(self):
        g = TestGraph()
        prog = '''
Client
    agent: Agent

Agent
'''
        #make_python(prog) #DEBUG
        exec(compile_fargish(prog), globals())
        client = g.make_node(Client)
        g.do_timestep()
        self.assertEqual(len(g), 2)
        agent = g.neighbor(client, port_label='agents')
        self.assertEqual(g.class_of(agent), Agent)
        self.assertTrue(g.has_hop(agent, 'behalf_of', client, 'agents'))

        # Once the agent is built, the client should not build another.
        g.do_timestep()
        self.assertEqual(len(g), 2)

    def test_see_do(self):
        g = TestGraph()
        prog = '''
tags -- taggees

Tag
Found, Done : Tag

Target
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
'''
        make_python(prog) #DEBUG
