import unittest

from Model import AtCell, C, Fizzle, I, I1, I2, I3, Lt, L, L1, L2, L3, \
    Plus, Subst, bottom_subst, empty_subst, UndefinedVariable, Succ, Seq, \
    Model, Rule, SideTag, WorldTag, W, C1, C2, OtherSide, OldWorld, NewWorld, \
    Canvas, Lhs, Rhs, SuccPair, J

from Log import lo, trace
from util import pr


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
        c1 = Canvas('c1')
        lhs = Lt(AtCell, C, I, L)
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

    def test_pmatch_succ_of(self) -> None:
        su = empty_subst.pmatch(Succ(L), 'b')
        self.assertEqual(su.eval(L), 'a')

    def test_pmatch_multiple_items(self) -> None:
        c1 = Canvas('c1')
        su = empty_subst.pmatch(
            [
                Lt(AtCell, C, I1, L1),
                Lt(Seq, C, Succ, Plus(I1, 1), I2, Succ(L1), L2)
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

    def test_pmatch_cant_match_letter_var_to_index(self) -> None:
        self.assertTrue(empty_subst.pmatch(L, 1).is_bottom())

    def test_pmatch_cant_match_index_var_to_letter(self) -> None:
        self.assertTrue(empty_subst.pmatch(I, 'a').is_bottom())

    def test_pmatch_plus_type_clash(self) -> None:
        self.assertTrue(empty_subst.pmatch(Plus(I, 1), 'a').is_bottom())

    def test_pmatch_plus_should_fail_on_1(self) -> None:
        self.assertTrue(empty_subst.pmatch(Plus(I, 1), 1).is_bottom())

    def test_pmatch_w(self) -> None:
        su = empty_subst.pmatch(W, OldWorld())
        self.assertTrue(su.eval(W), OldWorld())

    def xtest_pmatch_head(self) -> None:
        '''DETPAINTER'''

class TestEval(unittest.TestCase):

    def test_eval_undefined_variable(self) -> None:
        with self.assertRaises(UndefinedVariable):
            empty_subst.eval(I)

    def test_eval_letter(self) -> None:
        self.assertEqual(empty_subst.eval('a'), 'a')

    def test_eval_index(self) -> None:
        self.assertEqual(empty_subst.eval(1), 1)

    # TODO test_eval_canvas

    def test_eval_item(self) -> None:
        c1 = Canvas('c1')
        su = Subst.from_tups((C, c1), (I, 1), (L, 'a'))
        self.assertEqual(su.eval(Lt(AtCell, C, I, L)), AtCell(c1, 1, 'a'))

class TestCanvas(unittest.TestCase):

    def test_abc(self) -> None:
        m = Model()
        c1 = m.add_canvas('canvas1', 'abc')
        self.assertCountEqual(
            m.ws,
            [
                AtCell(c1, 1, 'a'),
                AtCell(c1, 2, 'b'),
                AtCell(c1, 3, 'c'),
                c1
            ]
        )

    # TODO something with canvas length

class TestRule(unittest.TestCase):

    rule_succ = [
        Rule(
            (Lt(AtCell, C, I, L), Lt(AtCell, C, Plus(I, 1), Succ(L))),
            Lt(Seq, C, Succ, I, Plus(I, 1), L, Succ(L))
        )
    ]

    rules5 = [
        Rule(
            (Lt(AtCell, C, I, L), Lt(AtCell, C, Plus(I, 1), Succ(L))),
            Lt(Seq, C, Succ, I, Plus(I, 1), L, Succ(L))
        ),
        Rule(
            (Lt(AtCell, C, I, L),
             Lt(Seq, C, Succ, Plus(I1, 1), I2, Succ(L1), L2)
            ),
            Lt(Seq, C, Succ, I1, I2, L1, L2)
        ),
        Rule(
            (Lt(Seq, C, Succ, I1, I2, L1, L2),
             Lt(AtCell, C, Plus(I2, 1), Succ(L2))),
            Lt(Seq, C, Succ, I1, I2, L1, L2)
        ),
        Rule(  # Seq + Seq (no overlap)
            (Lt(Seq, C, Succ, I1, I2, L1, L2),
             Lt(Seq, C, Succ, Plus(I2, 1), I3, Succ(L2), L3)),
            (Lt(Seq, C, Succ, I1, I3, L1, L3))
        ),
        Rule(  # Seq + Seq (overlap at one letter)
            (Lt(Seq, C, Succ, I1, I2, L1, L2),
             Lt(Seq, C, Succ, I2, I3, L2, L3)),
            (Lt(Seq, C, Succ, I1, I3, L1, L3))
        )
    ]

    def test_make_seq_from_2_consecutive_letters(self) -> None:
        c1 = Canvas('c1')
        rule = Rule(
            (Lt(AtCell, C, I, L), Lt(AtCell, C, Plus(I, 1), Succ(L))),
            Lt(Seq, C, Succ, I, Plus(I, 1), L, Succ(L))
        )
        self.assertEqual(
            rule.run([AtCell(c1, 1, 'a'), AtCell(c1, 2, 'b')]),
            Seq(c1, Succ, 1, 2, 'a', 'b')
        )

    def test_try_all_rules(self) -> None:
        rules = [
            Rule(
                (Lt(AtCell, C, I, L), Lt(AtCell, C, Plus(I, 1), Succ(L))),
                Lt(Seq, C, Succ, I, Plus(I, 1), L, Succ(L))
            )
        ]
        m = Model(rules)
        c1 = m.add_canvas('canvas1', 'abc')
        self.assertCountEqual(
            m.try_all_rules(),
            [
                Seq(c1, Succ, 1, 2, 'a', 'b'),
                Seq(c1, Succ, 2, 3, 'b', 'c')
            ]
        )

    def test_abc_to_seq(self) -> None:
        #NEXT add logging of which Rule matched
        m = Model(self.rules5)
        c1 = m.add_canvas('c1', 'abc')
        m.do_timestep(3)
        self.assertIn(Seq(c1, Succ, 1, 3, 'a', 'c'), m.ws)

    def xtest_succ_always_left_to_right(self) -> None:
        # TODO After ijl is working
        rules = [
            # C.I=L, C.J=Succ(L) -> SuccPair[C, I, C, J, L, Succ(L)]
            Rule(
                (Lt(AtCell, C, I, L), Lt(AtCell, C, J, Succ(L))),
                Lt(SuccPair, C, I, C, J, L, Succ(L))
            )
        ]
        m = Model(rules)
        c1 = m.add_canvas('c1', 'aba')
        m.do_timestep(2)
        self.assertIn(
            SuccPair(c1, 1, c1, 2, 'a', 'b'),
            m.ws
        )
        self.assertNotIn(
            SuccPair(c1, 3, c1, 2, 'a', 'b'),
            m.ws
        )

class TestMakingPainters(unittest.TestCase):

    def test_detect_otherside(self) -> None:
        # See that 'abc' is on the other side from 'abd'.
        rule = Rule(
            (Lt(SideTag, C1, Lhs()), Lt(SideTag, C2, Rhs()),
             Lt(WorldTag, C1, W), Lt(WorldTag, C2, W)),
            Lt(OtherSide, C1, C2)
        )
        m = Model([rule])
        c1 = m.add_canvas('c1', 'abc', side=Lhs(), world=OldWorld())
        c2 = m.add_canvas('c2', 'abd', side=Rhs(), world=OldWorld())
        m.do_timestep()
        self.assertIn(OtherSide(c1, c2), m.ws)

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

    def xtest_run_arrow(self) -> None:
        '''Run the arrow pointer from 'abc->xyz' on 'ijk->?' '''

    def xtest_detect_that_repeater_could_fill_c4(self) -> None:
        '''Continues previous test.'''

    def xtest_repeater_fills_canvas(self) -> None:
        '''r4 should c4'''
