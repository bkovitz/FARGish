import unittest

from Model import AtCell, C, Fizzle, I, I1, I2, Item, L, L1, L2, Plus, Subst, \
    bottom_subst, empty_subst, UndefinedVariable, Succ, Seq

class TestPmatch(unittest.TestCase):

    def test_pmatch_int_int(self) -> None:
        self.assertEqual(
            empty_subst.pmatch(1, 1),
            empty_subst
        )

    def test_pmatch_int_int_bad(self) -> None:
        self.assertEqual(
            empty_subst.pmatch(1, 2),
            bottom_subst
        )

    def test_pmatch_one_item(self) -> None:
        c1 = 'canvas'
        lhs = Item(AtCell, C, I, L)
        rhs = AtCell(c1, 2, 'b')
        self.assertEqual(
            empty_subst.pmatch(lhs, rhs),
            Subst.from_tups((C, c1), (I, 2), (L, 'b'))
        )

    def test_pmatch_try_to_reassign_variable(self) -> None:
        su = Subst.from_tups((I, 1))
        self.assertTrue(su.pmatch(I, 2).is_bottom())

    def test_pmatch_plus_one(self) -> None:
        su = empty_subst.pmatch(Plus(I, 1), 3)
        self.assertEqual(su.eval(I), 2)

    #TODO revise type taxonomy:  RuleElem, WorkspaceItem?
    
    def test_eval_undefined_variable(self) -> None:
        with self.assertRaises(UndefinedVariable):
            empty_subst.eval(I)

    def test_pmatch_succ_of(self) -> None:
        su = empty_subst.pmatch(Succ(L), 'b')
        self.assertEqual(su.eval(L), 'a')

    def test_pmatch_multiple_items(self) -> None:
        c1 = 'canvas'
        su = empty_subst.pmatch(
            [
                Item(AtCell, C, I1, L1),
                Item(Seq, C, Succ, Plus(I1, 1), I2, Succ(L1), L2)
            ],
            [
                AtCell(c1, 1, 'a'),
                Seq(c1, Succ, 2, 3, 'b', 'c')
            ]
        )
        self.assertEqual(
            su,
            Subst.from_tups((C, c1), (I1, 1), (L1, 'a'), (I2, 3), (L2, 'c'))
        )

    def xtest_pmatch_type_clash(self) -> None:
        '''try matching a letter to an index: should raise exception'''

    def xtest_pmatch_head(self) -> None:
        '''DETPAINTER'''

class TestMakingPainters(unittest.TestCase):

    def xtest_detect_otherside(self) -> None:
        '''See that 'abc' is on the other side from 'abd'.'''

    def xtest_convert_painter_item_to_cluster(self) -> None:
        '''AddExceptionPainter'''
        # Is this really needed?

    def xtest_abc_to_repeat(self) -> None:
        '''
        Make canvas containing 'abc'. Get a Repeat for the whole canvas.
        Many Seq objects may be created during this test.
        '''

    def xtest_abc_abd(self) -> None:
        '''
        Run the differ to make the painter that makes one repeater from
        another.
        '''

    def xtest_abd_to_repeat(self) -> None:
        '''Starting from 'abd, get Repeat with SeqSkip.'''


class TestRunningPainters(unittest.TestCase):

    def xtest_paint_letter_to_canvas(self) -> None:
        '''Start with 'abc' and paint a 'g' over the 'b'.'''

    def xtest_detect_that_arrow_can_run(self) -> None:
        '''Have abc->abd painter, have ijk repeater, see that we could run
        the painter on c3 to make a repeater for c4.'''

    def xtest_detect_that_repeater_could_fill_c4(self) -> None:
        '''Continues previous test.'''

    def xtest_repeater_fills_canvas(self) -> None:
        '''r4 should c4'''
