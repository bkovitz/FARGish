import unittest

from Hierarchy import Hierarchy

class TestHierarchy(unittest.TestCase):

    def test_basics(self):
        h = Hierarchy()
        self.assertFalse(h.isa('child1', 'parent'))

        h.parent('parent', 'child1')
        self.assertTrue(h.isa('child1', 'child1'))
        self.assertTrue(h.isa('child1', 'parent'))
        self.assertFalse(h.isa('parent', 'child1'))

        h.parent('grandparent', 'parent')
        self.assertTrue(h.isa('child1', 'grandparent'))

        h.parent('parent', 'child2')
        self.assertTrue(h.isa('child2', 'grandparent'))
