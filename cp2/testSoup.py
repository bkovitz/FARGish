# testSoup.py -- Unit tests for Soup.py

import unittest
import inspect

#from Addrs import I
##from Subst import Subst, Plus
#from Soup import Soup
#from Funcs import succ, same

from Model import I, Soup, succ, same


class TestSoup(unittest.TestCase):
    pass

# Commented out to avoid circular important, and nothing seems to call
# Soup.is_match().
#    def test_is_match_1(self) -> None:
#        soup = Soup()
#        self.assertEqual(
#            soup.is_match((I, Plus(I, 2), succ), (1, 3, succ)),
#            Subst.make_from((I, 1))
#        )
#
#    def test_is_match_2(self) -> None:
#        soup = Soup()
#        self.assertFalse(soup.is_match((I, Plus(I, 2), succ), (1, 3, same)))
#        
