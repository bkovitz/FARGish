# testModel.py -- Unit tests for Model.py

import unittest

from Model import a, succrule, Canvas, Chunk, Run, Seed, Length, Rule, L, Succ

if 'unittest.util' in __import__('sys').modules:
    # Show full diff in self.assertEqual.
    __import__('sys').modules['unittest.util']._MAX_LENGTH = 999999999


class TestSpanToChunk(unittest.TestCase):

    maxDiff = None

    def test_abc_to_chunk_with_run(self) -> None:
        canvas = Canvas.from_str('abc')
        span = canvas.span(1, 3)
        chunk = Run.make_run_chunk(span)
        self.assertEqual(
            chunk,
            #Chunk(span=span, body=(Run(succrule), Seed(a), Length(3)))
            span.Chunk(Run(succrule), Seed(a), Length(3))
        )

    def test_see_succ(self) -> None:
        #self.assertEqual(see_relation('a', 'b') , Rule(L, Succ(L)))
        self.assertEqual(Succ.elems_to_rule('a', 'b') , Rule(L, Succ(L)))

    #TODO We need to set up detection of a Run as a rewrite rule, maybe
    #in a class called RRule.
