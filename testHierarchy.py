import unittest

from Hierarchy import Hierarchy

class TestHierarchy(unittest.TestCase):

    def test_hierarchy(self):
        h = Hierarchy()
        self.assertFalse(h.isa('child1', 'parent'))

        h.declare_parent('parent', 'child1')
        self.assertTrue(h.isa('child1', 'child1'))
        self.assertTrue(h.isa('child1', 'parent'))
        self.assertFalse(h.isa('parent', 'child1'))

        h.declare_parent('grandparent', 'parent')
        self.assertTrue(h.isa('child1', 'grandparent'))

        h.declare_parent('parent', 'child2')
        self.assertTrue(h.isa('child2', 'grandparent'))

    def test_ascending_from(self):
        h = Hierarchy()

        h.declare_parent('grandparent', 'parent1', 'parent2')
        h.declare_parent('parent1', 'child1-1', 'child1-2')
        h.declare_parent('parent2', 'child2-1', 'child2-2')

        self.assertEqual(
            list(h.ascending_from('grandparent')),
            ['grandparent']
        )
        self.assertEqual(
            list(h.ascending_from('parent1')),
            ['parent1', 'grandparent']
        )
        self.assertEqual(
            list(h.ascending_from('child2-1')),
            ['child2-1', 'parent2', 'grandparent']
        )
