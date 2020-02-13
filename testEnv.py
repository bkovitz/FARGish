import unittest

from Env import Env
from exc import FARGishCompilerException

class TestEnv(unittest.TestCase):

    def testBasics(self):
        env = Env()
        self.assertEqual(env.get('x'), None)
        env.add('x', 'X-VALUE')
        self.assertEqual(env.get('x'), 'X-VALUE')
        env.push()
        self.assertEqual(env.get('x'), 'X-VALUE')
        env.add('y', 'Y-VALUE')
        self.assertEqual(env.get('y'), 'Y-VALUE')
        env.add('x', 'LOCAL-X-VALUE')
        self.assertEqual(env.get('x'), 'LOCAL-X-VALUE')
        env.pop()
        self.assertEqual(env.get('y'), None)
        self.assertEqual(env.get('x'), 'X-VALUE')

    def testRedefinitionError(self):
        env = Env()
        env.add('x', 'Value1')
        env.add('x', 'Value1')
        #TODO .add should generate more specific exception
        with self.assertRaises(FARGishCompilerException):
            env.add('x', 'Value2')
        self.assertEqual(env.get('x'), 'Value1')
