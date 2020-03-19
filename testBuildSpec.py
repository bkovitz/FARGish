# testBuildSpec.py -- Unit tests for BuildSpec

import unittest

from BuildSpec import to_args_kwargs, _Literal

class TestBuildSpec(unittest.TestCase):

    def test_to_args_kwargs1(self):
        tups = [('_args', 0)]
        lis = ['first']
        args, kwargs = to_args_kwargs(lis, tups)
        self.assertEqual(args, ['first'])
        self.assertEqual(kwargs, {})

    def test_to_args_kwargs2(self):
        tups = [('taggees', 0)]
        lis = ['first']
        args, kwargs = to_args_kwargs(lis, tups)
        self.assertEqual(args, [])
        self.assertEqual(kwargs, {'taggees': 'first'})

    def test_to_args_kwargs3(self):
        tups = [('operands', 0), ('operands', 1)]
        lis = ['one', 'two']
        args, kwargs = to_args_kwargs(lis, tups)
        self.assertEqual(args, [])
        self.assertEqual(kwargs, {'operands': ['one', 'two']})

    def test_to_args_kwargs4(self):
        tups = [('operands', 0), ('operands', 1), ('operands', 2)]
        lis = ['one', 'two', 'three']
        args, kwargs = to_args_kwargs(lis, tups)
        self.assertEqual(args, [])
        self.assertEqual(kwargs, {'operands': ['one', 'two', 'three']})

    def test_to_args_kwargs4(self):
        tups = [('operands', [0, 1, 2])]
        lis = ['one', 'two', 'three']
        args, kwargs = to_args_kwargs(lis, tups)
        self.assertEqual(args, [])
        self.assertEqual(kwargs, {'operands': ['one', 'two', 'three']})

    def test_to_args_kwargs5(self):
        tups = [('_args', [0, 1])]
        lis = ['first', 'second']
        args, kwargs = to_args_kwargs(lis, tups)
        self.assertEqual(args, ['first', 'second'])
        self.assertEqual(kwargs, {})

    def test_to_args_kwargs6(self):
        tups = [
            ('_args', _Literal('abc')),
            ('_args', 1),  # 'second'
            ('operands', [_Literal('def'), 0])
        ]
        lis = ['first', 'second']
        args, kwargs = to_args_kwargs(lis, tups)
        self.assertEqual(args, ['abc', 'second'])
        self.assertEqual(kwargs, {'operands': ['def', 'first']})
