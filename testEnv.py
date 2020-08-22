import unittest

from Env import Env
from exc import FARGishCompilerException

class TestEnv(unittest.TestCase):

    def test_env(self):
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

    def test_redefinition_error(self):
        env = Env()
        env.add('x', 'Value1')
        env.add('x', 'Value1')
        #TODO .add should generate more specific exception
        with self.assertRaises(FARGishCompilerException):
            env.add('x', 'Value2')
        self.assertEqual(env.get('x'), 'Value1')

    def test_gensym(self):
        env = Env()
        name1 = env.gensym('x')
        env.add(name1, 'VALUE1')
        name2 = env.gensym('x')
        env.add(name2, 'VALUE2')
        self.assertEqual(env[name1], 'VALUE1')
        self.assertEqual(env[name2], 'VALUE2')

        with env:  # env.push()
            name3 = env.gensym('x')
            env.add(name3, 'VALUE3')
            self.assertEqual(env[name1], 'VALUE1')
            self.assertEqual(env[name2], 'VALUE2')
            self.assertEqual(env[name3], 'VALUE3')
            # implicit env.pop(), exiting 'with'

        self.assertEqual(env[name1], 'VALUE1')
        self.assertEqual(env[name2], 'VALUE2')
        self.assertEqual(env.get(name3), None)
