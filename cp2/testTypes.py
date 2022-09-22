# testTypes.py -- Unit tests for Types.py

import unittest
import inspect

from Types import Annotation, Annotations, AnnotationType, CellBundle, Start, \
    empty_annotations


class TestTypes(unittest.TestCase):

    def test_in_annotations(self) -> None:
        anns = Annotations.make_from(Start)
        self.assertFalse(Start in empty_annotations)
        self.assertTrue(Start in anns)

    def test_cell_bundle_make_from(self) -> None:
        got = CellBundle.make_from()
        self.assertEqual(got, CellBundle(None, empty_annotations))
        got = CellBundle.make_from('j')
        self.assertEqual(got, CellBundle('j', empty_annotations))
        got = CellBundle.make_from('j', Start)
        self.assertEqual(got, CellBundle('j', Annotations.make_from(Start)))
        got = CellBundle.make_from('j', Annotations.make_from(Start))
        self.assertEqual(got, CellBundle('j', Annotations.make_from(Start)))
        got2 = CellBundle.make_from(got)
        self.assertEqual(got, got2)
