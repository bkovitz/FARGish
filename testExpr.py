# testExpr.py -- Unit tests for Expr

from expr import *

import unittest

class TestExpr(unittest.TestCase):

    def test_str(self):
        e1 = Equation(
                Plus(
                    Number(2),
                    Times(Number(3), Number(4))),
                Times(
                    Plus(
                        Number(1),
                        Number(5)),
                    Number(6)))
        self.assertEqual(str(e1), '2 + 3 * 4 = (1 + 5) * 6')

        e2 = Equation(
                Plus(
                    Times(Number(3), Number(4)),
                    Number(2)),
                Number(24))
        self.assertEqual(str(e2), '3 * 4 + 2 = 24')

        e3 = Equation(
                Minus(
                    Times(
                        Number(3),
                        Number(4)),
                    Plus(
                        Number(1),
                        Number(5))),
                Minus(
                    Plus(
                        Number(11),
                        Number(8)),
                    Times(
                        Number(2),
                        Number(7))))
        self.assertEqual(str(e3), '3 * 4 - (1 + 5) = (11 + 8) - 2 * 7')
