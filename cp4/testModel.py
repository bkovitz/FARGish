# testModel.py -- Unit tests for Model.py

import unittest
import inspect

from Model import Canvas, detect_repetition, Seed, Succ, Same, Pred, Repeat, \
    Skip, Workspace, PainterCluster, Define, OtherSide, Lhs, Rhs, \
    OldWorld, NewWorld

from Log import lo, set_log_level
from util import pts, reseed, short


class TestCanvas(unittest.TestCase):

    def test_str(self) -> None:
        canvas = Canvas.make_from('abc')
        self.assertEqual(str(canvas), 'abc')

    def test_len(self) -> None:
        canvas = Canvas.make_from('abc')
        self.assertEqual(canvas.length, 3)

class TestRepetitionDetection(unittest.TestCase):

    def test_abc(self) -> None:
        canvas = Canvas.make_from('abc')
        self.assertEqual(
            detect_repetition(canvas),
            Repeat(canvas, Seed('a', 1), Succ)
        )

    def test_cba(self) -> None:
        canvas = Canvas.make_from('cba')
        self.assertEqual(
            detect_repetition(canvas),
            Repeat(canvas, Seed('c', 1), Pred)
        )

    def test_eee(self) -> None:
        canvas = Canvas.make_from('eee')
        self.assertEqual(
            detect_repetition(canvas),
            Repeat(canvas, Seed('e', 1), Same)
        )

    def test_on_canvas_with_unknown_length(self) -> None:
        canvas = Canvas.make_unknown()
        self.assertIsNone(detect_repetition(canvas))

    def test_abd(self) -> None:
        canvas = Canvas.make_from('abd')
        self.assertEqual(
            detect_repetition(canvas),
            Repeat(canvas, Seed('a', 1), Succ, exception=Skip(3))
        )

class TestRepeat(unittest.TestCase):

    def test_repeat_succ(self) -> None:
        canvas = Canvas.make_unknown(length=3)
        repeat = Repeat(canvas, Seed('a', 1), Succ)
        ws = Workspace()
        repeat.fill(ws)
        self.assertEqual(str(canvas), 'abc')

    def test_repeat_succ_seed_2(self) -> None:
        canvas = Canvas.make_unknown(length=3)
        repeat = Repeat(canvas, Seed('j', 2), Succ)
        ws = Workspace()
        repeat.fill(ws)
        self.assertEqual(str(canvas), 'ijk')

    def test_repeat_succ_with_skip(self) -> None:
        canvas = Canvas.make_unknown(length=3)
        repeat = Repeat(canvas, Seed('a', 1), Succ, exception=Skip(3))
        ws = Workspace()
        repeat.fill(ws)
        self.assertEqual(str(canvas), 'abd')

    def test_repeat_succ_seed_3_with_skip_at_2(self) -> None:
        canvas = Canvas.make_unknown(length=3)
        repeat = Repeat(canvas, Seed('k', 3), Succ, exception=Skip(2))
        ws = Workspace()
        repeat.fill(ws)
        self.assertEqual(str(canvas), 'hjk')

    #TODO Skip right at the seed
    #TODO What should be the result of Skip(1)?


class TestWorkspace(unittest.TestCase):

    def test_define(self) -> None:
        ws = Workspace()
        ws.define('I1', 1)
        self.assertEqual(ws['I1'], 1)
        self.assertEqual(ws.get_index('I1'), 1)

    def test_repeat_succ(self) -> None:
        ws = Workspace()
        ws.define('S1', Canvas.make_unknown(length=3))
        ws.define('R1', Repeat('S1', 'D1', 'F1'))
        ws.define('D1', Seed('L1', 'I1'))
        ws.define('L1', 'a')
        ws.define('I1', 1)
        ws.define('F1', Succ)
        ws.run_repeater('R1')
        self.assertEqual(str(ws['S1']), 'abc')

#    def test_succ(self) -> None:
#        ws = Workspace()
#        ws.define('C0', Canvas.make_from('ax '))
#        #ws.run_painter(Succ(C0.1, C0.3))
#        ws.run_painter(Succ(Addr('C0', 1), Addr('C0', 3)))
#        self.assertEqual(str(ws['C0']), 'axb')

    def test_other_side_lhs_blank(self) -> None:
        ws = Workspace()
        ws.define('S1', Canvas.make_from('abc'), tag=[Lhs(), OldWorld()])
        ws.define('S2', Canvas.make_from('abd'), tag=[Rhs(), OldWorld()])
        ws.run_painter(OtherSide('S1', 'SS'))
        self.assertEqual(ws['SS'], ws['S2'])

    def test_other_side_blank_lhs(self) -> None:
        ws = Workspace()
        ws.define('S1', Canvas.make_from('abc'), tag=[Lhs(), OldWorld()])
        ws.define('S2', Canvas.make_from('abd'), tag=[Rhs(), OldWorld()])
        ws.run_painter(OtherSide('SS', 'S1'))
        self.assertEqual(ws['SS'], ws['S2'])

    def test_other_side_rhs_blank(self) -> None:
        ws = Workspace()
        ws.define('S1', Canvas.make_from('abc'), tag=[Lhs(), OldWorld()])
        ws.define('S2', Canvas.make_from('abd'), tag=[Rhs(), OldWorld()])
        ws.run_painter(OtherSide('S2', 'SS'))
        self.assertEqual(ws['SS'], ws['S1'])

    def test_other_side_blank_rhs(self) -> None:
        ws = Workspace()
        ws.define('S1', Canvas.make_from('abc'), tag=[Lhs(), OldWorld()])
        ws.define('S2', Canvas.make_from('abd'), tag=[Rhs(), OldWorld()])
        ws.run_painter(OtherSide('SS', 'S2'))
        self.assertEqual(ws['SS'], ws['S1'])

    def test_other_side2(self) -> None:
        ws = Workspace()
        ws.define('S1', Canvas.make_from('abc'), tag=[Lhs(), OldWorld()])
        ws.define('S2', Canvas.make_from('wrong'))
        ws.define('S3', Canvas.make_from('wrong'), tag=OldWorld())
        ws.define('SR', Canvas.make_from('abd'), tag=[Rhs(), OldWorld()])
        ws.run_painter(OtherSide('S1', 'SS'))
        self.assertEqual(ws['SS'], ws['SR'])

    def test_other_side_two_worlds(self) -> None:
        ws = Workspace()
        ws.define('S1', Canvas.make_from('abc'), tag=[Lhs(), OldWorld()])
        ws.define('S2', Canvas.make_from('abd'), tag=[Rhs(), OldWorld()])
        ws.define('S3', Canvas.make_from('ijk'), tag=[Lhs(), NewWorld()])
        ws.define('S4', Canvas.make_from('ijl'), tag=[Rhs(), NewWorld()])
        ws.run_painter(OtherSide('S3', 'SS'))
        self.assertEqual(ws['SS'], ws['S4'])
        ws.run_painter(OtherSide('S1', 'ST'))
        self.assertEqual(ws['ST'], ws['S2'])

#    def test_painter_cluster(self) -> None:
#        ws = Workspace()
#        ws.define('CLUSTER', PainterCluster(
#            Define('RR', Repeat('SS', 'DD', 'FF'))
#        ))
#        ws.define('S1', Canvas.make_unknown(length=3))
#        ws.define('D1', Seed('L1', 'I1'))
#        ws.define('L1', 'a')
#        ws.define('I1', 1)
#
##        self.assertEqual(
##            ws.get_painter_cluster('CLUSTER').params(),
##            ['SS', 'DD', 'FF']
##        )
#
#        ws.run_painter_cluster('CLUSTER', dict(SS='S1', DD='D1', FF=Succ))
#        self.assertEqual(str(ws['S1']), 'abc')

    def test_seed_params(self) -> None:
        self.assertCountEqual(Seed('L1', 'I1').params(), ['L1', 'I1'])

    def test_define_letter(self) -> None:
        ws = Workspace()
        name1 = ws.define_letter('a')
        name2 = ws.define_letter('b')
        self.assertEqual(name1, 'L1')
        self.assertEqual(ws[name1], 'a')
        self.assertEqual(name2, 'L2')
        self.assertEqual(ws[name2], 'b')

    def test_define_letter_that_clashes_with_existing_lettername(self) -> None:
        ws = Workspace()
        ws.define('L1', 'a')
        name = ws.define_letter('b')
        self.assertEqual(ws['L1'], 'a')
        self.assertEqual(name, 'L2')
        self.assertEqual(ws[name], 'b')

    def test_painter_cluster_just_define(self) -> None:
        ws = Workspace()
        ws.define('CLUSTER', PainterCluster(
            Define('LL', 'a')
        ))

        ws.run_painter_cluster('CLUSTER', dict())
        self.assertEqual(ws['L1'], 'a')

    def test_painter_cluster_just_define_after_existing_letter(self) -> None:
        ws = Workspace()
        ws.define('L1', 'b')
        ws.define('CLUSTER', PainterCluster(
            Define('LL', 'a')
        ))

        ws.run_painter_cluster('CLUSTER', dict())
        self.assertEqual(ws.all_letter_defs(), {'L1': 'b', 'L2': 'a'})

    def test_painter_cluster_assign_to_existing_letter(self) -> None:
        ws = Workspace()
        ws.define('L1', 'a')
        ws.define('CLUSTER', PainterCluster(
            Define('LL', 'a')
        ))
        ws.run_painter_cluster('CLUSTER', dict(LL='L1'))
        # This should do nothing, since L1 already exists and is 'a'
        #lo('UT', ws.subst)
        #self.assertEqual(len(ws.subst), 2)  # TODO rewrite more clearly
        self.assertEqual(ws.all_letter_defs(), {'L1': 'a'})

    def test_painter_cluster_local_same_name_as_existing_variable(self) -> None:
        ws = Workspace()
        ws.define('LL', 'a')
        lo('UT', ws.subst)
        ws.define('CLUSTER', PainterCluster(
            Define('LL', 'b')
        ))
        lo('UT2', ws.subst)
        ws.run_painter_cluster('CLUSTER', dict())
        lo('UT3', ws.subst)
        self.assertEqual(ws.all_letter_defs(), {'LL': 'a', 'L1': 'b'})

#    def test_simple_cluster(self) -> None:
#        ws = Workspace()
#        ws.define('CLUSTER', PainterCluster(
#            Define('DD1', Seed('LL1', 'II')),
#            Define('LL1', 'a'),
#            Define('DD2', Seed('LL2', 'II')),
#            Define('LL2', 'i')
#            # II is not defined: it will be filled in to be the same in both seeds
#        ))
#        ws.define('D1', Seed('L1', 'I1'))
#        ws.define('L1', 'a')
#        ws.define('I1', 1)
#        subst = ws.run_painter_cluster('CLUSTER', dict(DD1='D1'))
#
#        self.assertCountEqual(
#            ws['CLUSTER'].params(),
#            ['DD1', 'LL1', 'DD2', 'LL2', 'II']
#        )

#        self.assertEqual(
#            subst,
#            {
#                'DD1': Seed('LL1', 'II'),
#                'LL1': 'a',
#                'DD2': Seed('LL2', 'II'),
#                'LL2': 'i',
#                'II':  1
#            }
#        )

#        self.assertEqual(subst['DD1'], Seed('LL1', 'II'))
#        self.assertEqual(subst['LL1'], 'a')
#        self.assertEqual(subst['DD2'], Seed('LL2', 'II'))
#        self.assertEqual(subst['LL2'], 'i')
#        self.assertEqual(subst['II'], 1)

#        other_seeds: List[Variable] = ws.get_by_type(Seed)
#        seed2 = ws.??
#        self.assertEqual(ws.get_determinate(seed2), Seed('i', 1))

class TestParseInputString(unittest.TestCase):

    def test_abc_abd_ijk(self) -> None:
        parsed = Canvas.parse_analogy_string('abc->abd; ijk->?');

        expect_snippet1 = Canvas.make_from('abc')
        expect_snippet2 = Canvas.make_from('abd')
        expect_snippet3 = Canvas.make_from('ijk')
        expect_snippet4 = Canvas.make_unknown()

        got1, got2, got3, got4 = parsed
        self.assertEqual(str(got1), str(expect_snippet1))
        self.assertEqual(str(got2), str(expect_snippet2))
        self.assertEqual(str(got3), str(expect_snippet3))
        self.assertEqual(str(got4), str(expect_snippet4))

        # These canvases need "addresses": C.1, C.2, C.3, C.4
        # These canvases also need variables: S1, S2, S3, S4
        # There also need to be relations between these canvases: OtherSide
        # and OtherWorld.

#class TestDiffer(unittest.TestCase):
#
#    def test_diff_abc_abd(self) -> None:
#        ws = Workspace()
#        ws.make('S1', Canvas.make_from('abc'))
#        ws.make('R1', Repeat('S1', Seed('a', 1), Succ))
#        ws.make('S2', Canvas.make_from('abd'))
#        ws.make('R2', Repeat('S2', Seed('a', 1), Succ, exception=Skip(3)))
#
#        arrow = ws.construct_diff('R1', 'R2', name='Arrow')
#        self.assertEqual(
#            arrow.params, ['RR1', 'RR2', 'SS1', 'SS2', 'DD', 'FF', 'GG']
#        )
#        self.assertEqual(
#            arrow,
#            PainterCluster(
#                Define('RR1', Repeat('SS1', 'DD', 'FF')),
#                Define('RR2', Repeat('SS1', 'DD', 'FF', 'EE'),
#                OtherSide('SS1', 'SS2'),
#                Exception_('GG', 'II'),
#                Define('GG', Skip)
#                Define('II', 3)
#            )
#        )
