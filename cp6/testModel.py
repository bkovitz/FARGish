# testModel.py -- Unit tests for Model.py

import unittest

from Model import a, succrule, Canvas, Chunk, Run, Seed, Length, Rule, L, Succ

class TestSpanToChunk(unittest.TestCase):

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
