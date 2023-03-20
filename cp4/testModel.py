# testModel.py -- Unit tests for Model.py

import unittest
import inspect
from pprint import pp

from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check, get_type_hints, get_args

from Model import Canvas, detect_repetition, Seed, Succ, Same, Pred, Repeat, \
    Skip, Workspace, PainterCluster, Define, OtherSide, Lhs, Rhs, \
    OldWorld, NewWorld, Tag, Var, Variable, Argument, Subst

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

    def test_repeat_params(self) -> None:
        canvas = Canvas.make_unknown(length=3)
        repeat = Repeat('C0', 'D1', 'F1')
        self.assertCountEqual(repeat.params(), ['C0', 'D1', 'F1'])

    def test_repeat_params_with_exception(self) -> None:
        canvas = Canvas.make_unknown(length=3)
        repeat = Repeat('C0', 'D1', 'F1', 'E1')
        self.assertCountEqual(repeat.params(), ['C0', 'D1', 'F1', 'E1'])

    #TODO Skip right at the seed
    #TODO What should be the result of Skip(1)?


@dataclass(frozen=True)
class ArbitraryTag1(Tag):
    pass

@dataclass(frozen=True)
class ArbitraryTag2(Tag):
    pass

class TestWorkspace(unittest.TestCase):

    def test_define(self) -> None:
        ws = Workspace()
        ws.define('I1', 1)
        self.assertEqual(ws['I1'], 1)
        self.assertEqual(ws.get_index('I1'), 1)

    def test_define_compound_object_and_create_variables_automatically(self) -> None:
        ws = Workspace()
        ws.define('D1', Seed('a', 1))
        self.assertEqual(ws['D1'], Seed('L1', 'I1'))
        self.assertEqual(ws['L1'], 'a')
        self.assertEqual(ws['I1'], 1)

    def test_tags(self) -> None:
        ws = Workspace()
        ws.define('L1', 'a', tag=ArbitraryTag1())
        self.assertEqual(ws.find_objects_with_tag(ArbitraryTag1()), set(['a']))
        self.assertEqual(ws.find_object_with_tag(ArbitraryTag1()), 'a')
        self.assertEqual(ws.tags_of('a'), set([ArbitraryTag1()]))
        self.assertEqual(ws.tags_of('L1'), set([ArbitraryTag1()]))

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

    def test_painter_cluster_repeat(self) -> None:
        ws = Workspace()
        ws.define('CLUSTER', PainterCluster(
            Define('RR', Repeat('SS', 'DD', 'FF'))
        ))
        ws.define('S1', Canvas.make_unknown(length=3))
        ws.define('D1', Seed('L1', 'I1'))
        ws.define('L1', 'a')
        ws.define('I1', 1)

        self.assertCountEqual(
            ws.get_painter_cluster('CLUSTER').params(),
            ['RR', 'SS', 'DD', 'FF']
        )

        ws.run_painter_cluster('CLUSTER', dict(SS='S1', DD='D1', FF=Succ))
        # TODO Assert that F1=Succ  (no duplicate, i.e. F2)
        self.assertEqual(ws['R1'], Repeat('S1', 'D1', 'F1'))
        self.assertIsNone(ws['F2'])  # regression test: no duplicate Succ
        ws.run_repeater('R1')
        self.assertEqual(str(ws['S1']), 'abc')

    def test_seed_params(self) -> None:
        self.assertCountEqual(Seed('L1', 'I1').params(), ['L1', 'I1'])

    def test_define_letter(self) -> None:
        ws = Workspace()
        name1 = ws.define_and_name('a')
        name2 = ws.define_and_name('b')
        self.assertEqual(name1, 'L1')
        self.assertEqual(ws[name1], 'a')
        self.assertEqual(name2, 'L2')
        self.assertEqual(ws[name2], 'b')

    def test_define_letter_that_clashes_with_existing_lettername(self) -> None:
        ws = Workspace()
        ws.define('L1', 'a')
        name = ws.define_and_name('b')
        self.assertEqual(ws['L1'], 'a')
        self.assertEqual(name, 'L2')
        self.assertEqual(ws[name], 'b')

#    def test_define_seed(self) -> None:
#        ws = Workspace()
#        ws.define('D1', Seed('a', 1))
#        self.assertEqual(ws['D1'], Seed('L1', 'I1'))
#        self.assertEqual(ws['L1'], 'a')
#        self.assertEqual(ws['I1'], 1)

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
        # Running the PainterCluster should do nothing, since L1 already exists and
        # is 'a'.
        self.assertEqual(ws.all_letter_defs(), {'L1': 'a'})

    def test_painter_cluster_local_same_name_as_existing_variable(self) -> None:
        ws = Workspace()
        ws.define('LL', 'a')
        ws.define('CLUSTER', PainterCluster(
            Define('LL', 'b')
        ))
        ws.run_painter_cluster('CLUSTER', dict())
        self.assertEqual(ws.all_letter_defs(), {'LL': 'a', 'L1': 'b'})

#    def test_painter_cluster_create_seed(self) -> None:
#        ws = Workspace()
#        ws.define('CLUSTER', PainterCluster(
#            Define('DD', Seed('LL', 'II'))
#        ))
#        ws.run_painter_cluster('CLUSTER', dict(LL='a', II=1))
#        self.assertEqual(ws.all_seed_defs(), {'D1': Seed('L1', 'I1')})
#        self.assertEqual(ws.all_letter_defs(), {'L1': 'a'})
#        self.assertEqual(ws.all_index_defs(), {'I1': 1})

    def test_painter_cluster_create_seed2(self) -> None:
        ws = Workspace()
        ws.define('L1', 'a')
        ws.define('I1', 1)
        ws.define('CLUSTER', PainterCluster(
            Define('DD', Seed('LL', 'II'))
        ))
        ws.run_painter_cluster('CLUSTER', dict(LL='L1', II='I1'))
        self.assertEqual(ws.all_seed_defs(), {'D1': Seed('L1', 'I1')})

    #TODO Seed(LL, I1): the I1 is already level-0

    #TODO A failed PainterCluster: ws should be unmodified--no half-done
    #entering of newly created objects.

    #TODO Nested PainterClusters.

    #TODO Fill a cluster's variables "bottom-up"

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
#        subst_out = ws.run_painter_cluster('CLUSTER', dict(DD1='D1'))
#        pp(subst_out)
#
#        self.assertCountEqual(
#            ws['CLUSTER'].params(),
#            ['DD1', 'LL1', 'DD2', 'LL2', 'II']
#        )
#
#        pp(ws.subst)
#        self.assertEqual(ws['D2'], Seed('L2', 'I1'))
#        self.assertEqual(ws['L2'], 'i')

#        lo('UT', subst)
#
#        expect: List[Tuple[Variable, Argument]] = [
#            ('DD1', 'D1'),   #Seed('LL1', 'II')),
#            ('LL1', 'a'),
#            ('DD2', Var.at_level(Seed('LL2', 'II'), 1)),
#            ('LL2', 'i'),
#            ('II',  1)
#        ]
#
#        for name, value in expect:
#            name1 = Var.at_level(name, 1)
#            self.assertEqual(
#                subst[name1],
#                value,
#                f'subst[{name1!r}] was {subst[name1]}, not {value}'
#            )

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

#class TestArrow(unittest.TestCase):
#
#    def setUp(self) -> None:
#        self.ws = Workspace()
#        self.ws.define('ARROW', PainterCluster(
#            
#        ))

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

class TestSubst(unittest.TestCase):

    def test_unify_letter_with_itself(self) -> None:
        su = Subst()
        su = su.unify('a', 'a')
        self.assertFalse(su.is_bottom())
        self.assertEqual(su.eval('a'), 'a')

    def test_unify_index_with_itself(self) -> None:
        su = Subst()
        su = su.unify(1, 1)
        self.assertFalse(su.is_bottom())
        self.assertEqual(su.eval(1), 1)

    def test_unify_variable_with_itself(self) -> None:
        su = Subst()
        su = su.unify('L1', 'L1')
        self.assertFalse(su.is_bottom())
        #self.assertTrue(su.are_equal('L1', 'L1'))

    def test_unify_with_letter(self) -> None:
        su = Subst()
        su = su.unify('L1', 'a')
        self.assertEqual(su.eval('L1'), 'a')

    def test_unify_with_index(self) -> None:
        su = Subst()
        su = su.unify('I1', 1)
        self.assertEqual(su.eval('I1'), 1)

    def test_two_variables_are_unequal(self) -> None:
        su = Subst()
        self.assertFalse(su.are_equal('L1', 'L2'))

    def test_eval_undefined_variable(self) -> None:
        su = Subst()
        self.assertEqual(su.eval('L1'), None)

    def test_unify_variable_with_defined_variable(self) -> None:
        su = Subst()
        su = su.unify('L1', 'a')
        su = su.unify('L2', 'L1')
        self.assertEqual(su.eval('L1'), su.eval('a'))
        self.assertEqual(su.eval('L2'), su.eval('a'))
        self.assertEqual(su.eval('L1'), su.eval('L2'))

    def test_unify_variables_and_then_define_one_of_them(self) -> None:
        su = Subst()
        su = su.unify('L2', 'L1')
        su = su.unify('L1', 'a')
        self.assertEqual(su.eval('L1'), su.eval('a'))
        self.assertEqual(su.eval('L2'), su.eval('a'))
        self.assertEqual(su.eval('L1'), su.eval('L2'))

    '''
    LL=L1
    LL='a'

    L1='a'
    LL='a'
    '''

    def test_unify_seed_seed(self) -> None:
        su = Subst()
        su = su.unify(Seed('LL', 'II'), Seed('L1', 'I1'))
        #self.assertTrue(su.are_equal('LL', 'L1'))
        #self.assertTrue(su.are_equal('II', 'I1'))
        su = su.unify('LL', 'a')
        su = su.unify('I1', 1)
        self.assertEqual(su.eval(Seed('LL', 'II')), Seed('a', 1))
        self.assertEqual(su.eval(Seed('L1', 'I1')), Seed('a', 1))

    def test_unify_two_different_constants(self) -> None:
        su = Subst()
        su = su.unify('a', 'b')
        self.assertTrue(su.is_bottom())

    def test_unify_variables_with_two_different_constants(self) -> None:
        su = Subst()
        su = su.unify('L1', 'a')
        su = su.unify('L2', 'b')
        su = su.unify('L1', 'L2')
        self.assertTrue(su.is_bottom())

    def xtest_unify_DD1_D1(self) -> None:
        su = Subst()
        su = su.unify('DD1', Seed('LL1', 'II'))
        su = su.unify('D1', Seed('L1', 'I1'))
        su = su.unify('DD1', 'D1')
        # BUG: We don't yet unify what DD1 and D1 stand for.
        #self.assertTrue(su.are_equal('LL1', 'L1'))
        #self.assertTrue(su.are_equal('II', 'I1'))
        #self.assertTrue(su.are_equal('DD1', 'D1'))
        su1 = su.unify('LL1', 'a').unify('I1', 1)
        lo('SU1', su1)
        self.assertEqual(su1.eval('D1'), Seed('a', 1))
        self.assertEqual(su1.eval('DD1'), Seed('a', 1))

#    def test_unify_II_with_three_variables(self) -> None:
#        su = Subst()
#        su = su.unify('D1', Seed('L1', 'I1'))
#        su = su.unify('D2', Seed('L2', 'I2'))
#        su = su.unify('D3', Seed('L3', 'I3'))
#        su = su.unify('II', 'I1')
#        su = su.unify('II', 'I2')
#        su = su.unify('II', 'I3')
#        self.assertTrue(su.are_equal('II', 'I1'))
#        self.assertTrue(su.are_equal('II', 'I2'))
#        self.assertTrue(su.are_equal('II', 'I3'))
#        '''
#        I1=1, then II=1, I2=1, I3=1
#        II=1, then I1=1, I2=1, I3=1
#        '''
#        
#    def test_unify_DD1_DD2(self) -> None:
#        su = Subst()
#        # as in a PainterCluster:
#        su = su.unify('DD1', Seed('LL1', 'II'))
#        su = su.unify('LL1', 'a')
#        su = su.unify('DD2', Seed('LL2', 'II'))
#        su = su.unify('LL2', 'i')
#        # as in the Workspace:
#        su = su.unify('D1', Seed('L1', 'I1'))
#        su = su.unify('L1', 'a')
#        su = su.unify('I1', 1)
#        su = su.unify('DD1', 'D1')
#        self.assertEqual(su.eval('DD2'), Seed('i', 1))
#
#    def test_unify_occurs_check(self) -> None:
#        su = Subst()
#        su = su.unify('D1', Seed('D1', 1))
#        self.assertTrue(su.is_bottom())

    # TODO Creating a new object -- keep this separate from unification

    # TODO test that on exit from a PainterCluster, all local variables are
    # eliminated, all new objects are created and named, and all 'same' relations
    # between variables in the new objects are preserved.
