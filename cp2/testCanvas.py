# testCanvas.py -- Unit tests for Canvas.py

import unittest
import inspect

from Canvas import Canvas1D
from util import pts


class TestCanvas1D(unittest.TestCase):

    def test_all_indices_and_values(self) -> None:
        c = Canvas1D.make_from('aja')
        self.assertCountEqual(
            c.all_indices_and_values(),
            [(1, 'a'), (2, 'j'), (3, 'a')]
        )

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

#    def test_withann_start_end(self) -> None:
#        c = Canvas1D.make_from('ajaqb')
        
