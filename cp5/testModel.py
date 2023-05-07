import unittest

from Model import C, I, L, Item, AtCell, empty_subst, Subst

class TestPmatch(unittest.TestCase):

    def xtest_pmatch_int_int(self) -> None:
        pass

    def test_pmatch_one_item(self) -> None:
        c1 = 'canvas'
        lhs = Item(AtCell, C, I, L)
        rhs = AtCell(c1, 2, 'b')
        self.assertEqual(
            empty_subst.pmatch(lhs, rhs),
            Subst.from_tups((C, c1), (I, 2), (L, 'b'))
        )

    def xtest_pmatch_plus_one(self) -> None:
        pass

    def xtest_pmatch_succ_of(self) -> None:
        '''
        C.I=L, C.(I+1)=Succ(L) -> Seq[C Succ I I+1 L Succ(L)]
        '''

    def xtest_pmatch_multiple_items(self) -> None:
        pass

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
