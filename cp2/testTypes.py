# testTypes.py -- Unit tests for Types.py

import unittest
import inspect

from Model import Annotation, Annotations, AnnotationType, CellBundle, Start, \
    empty_annotations, Blank


class TestTypes(unittest.TestCase):

    def test_in_annotations(self) -> None:
        anns = Annotations.make_from(Start)
        self.assertFalse(Start in empty_annotations)
        self.assertTrue(Start in anns)

    def test_cell_bundle_make_from(self) -> None:
        got = CellBundle.make_from()
        self.assertEqual(got, CellBundle(Blank(), empty_annotations))

        got = CellBundle.make_from('j')
        self.assertEqual(got, CellBundle.make_from('j', empty_annotations))

        got = CellBundle.make_from('j', Start)
        self.assertEqual(got, CellBundle.make_from(
            'j', Annotations.make_from(Start))
        )

        got = CellBundle.make_from('j', Annotations.make_from(Start))
        self.assertEqual(got, CellBundle.make_from(
            'j', Annotations.make_from(Start))
        )

        got2 = CellBundle.make_from(got)
        self.assertEqual(got, got2)

    def test_cell_bundle_value_only(self) -> None:
        got = CellBundle.make_from()
        self.assertTrue(got.value_only())

        got = CellBundle.make_from('j')
        self.assertTrue(got.value_only())

        got = CellBundle.make_from('j', Start)
        self.assertFalse(got.value_only())

        got = CellBundle.make_from('j', Annotations.make_from(Start))
        self.assertFalse(got.value_only())

        got2 = CellBundle.make_from(got)
        self.assertFalse(got.value_only())
