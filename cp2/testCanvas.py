# testCanvas.py -- Unit tests for Canvas.py

import unittest
import inspect

from Canvas import Canvas1D
from util import pts

from Types import Annotation


MyAnnotation = Annotation('MyAnnotation')

class TestCanvas1D(unittest.TestCase):

    def test_set_and_get(self) -> None:
        c = Canvas1D.make_from('aja')
        self.assertEqual(c[1], 'a')  # 1-based indexing
        self.assertEqual(c.clarity(1), 5)
        c[1] = 'b'
        self.assertEqual(c[1], 'a'); self.assertEqual(c.clarity(1), 4)
        c[1] = 'b'
        self.assertEqual(c[1], 'a'); self.assertEqual(c.clarity(1), 3)
        c[1] = 'b'
        self.assertEqual(c[1], 'a'); self.assertEqual(c.clarity(1), 2)
        c[1] = 'b'
        self.assertEqual(c[1], 'a'); self.assertEqual(c.clarity(1), 1)
        c[1] = 'b'
        self.assertEqual(c[1], None); self.assertEqual(c.clarity(1), 0)
        c[1] = 'b'
        self.assertEqual(c[1], 'b'); self.assertEqual(c.clarity(1), 1)
        c[1] = 'b'
        self.assertEqual(c[1], 'b'); self.assertEqual(c.clarity(1), 2)
        
    def test_all_indices_and_values(self) -> None:
        c = Canvas1D.make_from('aja')
        self.assertCountEqual(
            c.all_indices_and_values(),
            [(1, 'a'), (2, 'j'), (3, 'a')]
        )

    '''
    def test_letter_match_with_start_end(self) -> None:
        c = Canvas1D.make_from('ajaqb')
        self.assertCountEqual(
            c.all_matching('a'),
            [1, 3]
        )
        self.assertCountEqual(
            c.all_matching('(a'),
            [1]
        )
        self.assertCountEqual(
            c.all_matching('b)'),
            [5]
        )
        self.assertCountEqual(
            c.all_matching('-a'),  # anchored to middle (not start or end)
            [3]
        )
    '''

    '''
    def test_paint_annotation(self) -> None:
        c = Canvas1D.make_from('ajaqb')
        c[2] = MyAnnotation
        self.assertEqual(c[2], 'j')
    '''



#    def test_withann_start_end(self) -> None:
#        c = Canvas1D.make_from('ajaqb')
        
