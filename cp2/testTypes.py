# testTypes.py -- Unit tests for Types.py

import unittest
import inspect

from Types import Annotation, Annotations, AnnotationType, Start, \
    empty_annotations


class TestTypes(unittest.TestCase):

    def test_in_annotations(self) -> None:
        anns = Annotations.make_from(Start)
        self.assertFalse(Start in empty_annotations)
        self.assertTrue(Start in anns)
