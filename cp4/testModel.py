# testModel.py -- Unit tests for Model.py

import unittest
import inspect
from pprint import pp
if 'unittest.util' in __import__('sys').modules:
    # Show full diff in self.assertEqual.
    __import__('sys').modules['unittest.util']._MAX_LENGTH = 999999999

from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check, get_type_hints, get_args

from Model import Canvas, detect_repetition, Seed, Succ, Same, Pred, Repeat, \
    Skip, Workspace, PainterCluster, Define, OtherSide, Lhs, Rhs, \
    OldWorld, NewWorld, Tag, Var, Variable, Argument, Subst, empty_subst, \
    DiffContext, NoValue, LengthPainter, ArgumentsFailRelation, Address, \
    is_variable, ArgumentRelationDetector, CanvasAddress, ParameterAddress, \
    PCMaker, At

from Log import lo, set_log_level
from util import first, pts, reseed, short


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
        ws.define('L1', 'a', tags=ArbitraryTag1())
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

    def test_seed_params(self) -> None:
        self.assertCountEqual(Seed('L1', 'I1').params(), ['L1', 'I1'])

    #TODO Seed(LL, I1): the I1 is already level-0

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

    def test_variable_of(self) -> None:
        ws = Workspace()
        ws.define('S1', Canvas.make_from('abc'))
        canvas = ws.eval('S1')
        assert isinstance(canvas, Canvas)
        self.assertEqual(ws.variable_of(canvas), 'S1')

    def test_parse_abc_abd_ijk(self) -> None:
        ws = Workspace()
        ws.parse_analogy_string('abc->abd; ijk->?')
        self.assertEqual(str(ws['S1']), 'abc')
        self.assertEqual(str(ws['S2']), 'abd')
        self.assertEqual(str(ws['S3']), 'ijk')
        self.assertEqual(str(ws['S4']), '')
        self.assertEqual(ws.tags_of('S1'), set([Lhs(), OldWorld()]))
        self.assertEqual(ws.tags_of('S2'), set([Rhs(), OldWorld()]))
        self.assertEqual(ws.tags_of('S3'), set([Lhs(), NewWorld()]))
        self.assertEqual(ws.tags_of('S4'), set([Rhs(), NewWorld()]))

        # TODO Do these canvases need "addresses"? C.1, C.2, C.3, C.4

    def test_repeater_for_canvas(self) -> None:
        ws = Workspace()
        ws.define('S1', Canvas.make_from('abc'))
        r = detect_repetition(ws.get_canvas('S1'))
        assert r is not None
        repeater_variable = ws.define_and_name(r)
        stored_repeater = ws[repeater_variable]
        assert isinstance(stored_repeater, Repeat)
        self.assertEqual(stored_repeater.canvas, 'S1')
        # This ensures that the generated repeater refers to the existing
        # canvas, i.e. no duplicate canvas gets created when the repeater
        # is put into the workspace.

    def test_add_canvas(self) -> None:
        ws = Workspace()
        var = ws.add_canvas('ab_')
        self.assertTrue(
            Canvas.are_identical(ws.get_canvas(var),
            Canvas.make_from('ab_'))
        )

    def test_make_succ(self) -> None:
        ws = Workspace()
        c1 = ws.add_canvas('ab_')
        left_address = CanvasAddress(c1, 1)
        right_address = CanvasAddress(c1, 2)
        ws.run_detector(Succ.examine_pair, At(left_address), At(right_address))
        got: Succ = first(ws.get_all(Succ))
        self.assertTrue(is_variable(got.left))
        self.assertTrue(is_variable(got.right))
        self.assertEqual(
            ws.eval(got),
            ws.eval(Succ(At(left_address), At(right_address)))
        )

class TestSucc(unittest.TestCase):

    def test_succ_examine_a_b(self) -> None:
        ws = Workspace()
        self.assertCountEqual(
            Succ.examine_pair(ws, 'a', 'b'),
            [Succ('a', 'b')]
        )
        
    def test_succ_examine_addrs(self) -> None:
        ws = Workspace()
        c1 = ws.add_canvas('xqv'); a1 = c1.addr(1); a2 = c1.addr(2)
        # the Succ relation here is between addresses, not letters
        self.assertCountEqual(
            Succ.examine_pair(ws, a1, a2),
            [Succ(a1, a2)]
        )
    
    def test_detect_arg_relation_in_succ(self) -> None:
        ws = Workspace()
        c1 = ws.add_canvas('ab_'); a1 = c1.addr(1); a2 = c1.addr(2)
        p1 = ws.define('P1', Succ(a1, a2))
        self.assertCountEqual(
            Succ.examine_pair(
                ws,
                At(ParameterAddress('P1', 'left')),
                At(ParameterAddress('P1', 'right'))
            ),
            [Succ(
                At(ParameterAddress('P1', 'left')),
                At(ParameterAddress('P1', 'right'))
            )]
        )

    # Should we have a function that, when passed two anythings, returns a
    # relation detected between them? E.g. (lhs, 1), (rhs, 2) ->
    # OtherSide(lhs, rhs), Succ(1, 2).
    # Then pass just the arguments within a painter (or two painters) and
    # this will find all the relations.

    # It should be possible to call PCMaker() with a pile of relations
    # and desired objects to rebuild, and PCMaker() assigns all needed
    # variables and creates the PainterCluster.

    # PCMaker replaces *all* constants with variables.
    # Maybe the caller can specify some constants to preserve, e.g. 'a'.
    # Therefore the caller can just pass in constants, and PCMaker will
    # figure out how to name them, giving the same name to constants
    # that are identical (except when the caller has provided an explicit
    # Same relation for them).

    # If an object has arguments, PCMaker assigns variables to those arguments.
    # If any two objects of an argument are the same, PCMaker assigns them
    # the same name (unless the caller requested an explicit Same). This
    # applies recursively to the arguments, which can themselves be objects.

    # If caller specifies "rebuild X", then PCMaker makes a Define for X.

    # Succ((c1, 1), (c1, 2))  'ab'   PP1=Succ(AA1, AA2)
    # (c1, 1)  (c1, 2)
    # Same(c1, c1)      SS=c1
    # Succ(1, 2)        II1=1, II2=2
    # PainterCluster(
    #   PP1=Succ(AA1, AA2)
    #   AA1=CanvasAddress(SS, II1)
    #   AA2=CanvasAddress(SS, II2)
    #   Succ(II1, II2)
    #)

class TestPCMaker(unittest.TestCase):

    def test_pcmaker_rebuild_seeds(self) -> None:
        ws = Workspace()
        pcm = PCMaker(ws)
        pcm.will_rebuild_object(Seed('a', 1))
        pcm.will_rebuild_object(Seed('j', 1))
        self.assertEqual(
            pcm.painter_cluster(),
            PainterCluster(
                Define('DD1', Seed('LL1', 'II')),
                Define('DD2', Seed('LL2', 'II')),
            )
        )

    def test_pcmaker_rebuild_seeds_with_succ(self) -> None:
        ws = Workspace()
        pcm = PCMaker(ws)
        pcm.will_rebuild_object(Seed('a', 1))
        pcm.will_rebuild_object(Seed('b', 1))
        pcm.will_rebuild_relation(Succ('a', 'b'))
        self.assertEqual(
            pcm.painter_cluster(),
            PainterCluster(
                Define('DD1', Seed('LL1', 'II')),
                Define('DD2', Seed('LL2', 'II')),
                Succ('LL1', 'LL2')
            )
        )

        # NEXT need to write tests for the code that calls PCMaker, specifying
        # the objects and relevant relations

class TestArgumentRelationDetector(unittest.TestCase):

    def xtest_detect_arg_relation_in_succ(self) -> None:
        ws = Workspace()
        c1 = ws.add_canvas('ab_'); a1 = c1.addr(1); a2 = c1.addr(2)
        p1 = ws.define('P1', Succ(a1, a2))
        self.assertCountEqual(
            ArgumentRelationDetector.examine_pair(
                ws,
                ParameterAddress('P1', 'left'),
                ParameterAddress('P1', 'right')
            ),
            #ArgumentRelationDetector.examine_painter('P1'),
            [PainterCluster(
                Define('PP1', Succ('AA1', 'AA2')),
                Define('AA1', CanvasAddress('SS', 'II1')),
                Define('AA2', CanvasAddress('SS', 'II2')),
                Succ('II1', 'II2')
            )]
            # "Given 1, 2, since they are successors, make Succ(1, 2)"
        )

    # TODO Succ relation across two painters

    def test_no_arg_relation(self) -> None:
        ws = Workspace()
        c1 = ws.add_canvas('abc'); a1 = c1.addr(1); a3 = c1.addr(3)
        p1 = ws.define('P1', Succ(a1, a3))
        self.assertCountEqual(
            ArgumentRelationDetector.examine_pair(
                ws,
                At(ParameterAddress('P1', 'left')),
                At(ParameterAddress('P1', 'right'))
            ),
            []
        )

class TestOtherSide(unittest.TestCase):

    def test_other_side_lhs_blank(self) -> None:
        ws = Workspace()
        ws.define('S1', Canvas.make_from('abc'), tags=[Lhs(), OldWorld()])
        ws.define('S2', Canvas.make_from('abd'), tags=[Rhs(), OldWorld()])
        ws.run_painter(OtherSide('S1', 'SS'))
        self.assertEqual(ws.eval('SS'), ws['S2'])

    def test_other_side_blank_lhs(self) -> None:
        ws = Workspace()
        ws.define('S1', Canvas.make_from('abc'), tags=[Lhs(), OldWorld()])
        ws.define('S2', Canvas.make_from('abd'), tags=[Rhs(), OldWorld()])
        ws.run_painter(OtherSide('SS', 'S1'))
        self.assertEqual(ws.eval('SS'), ws['S2'])

    def test_other_side_rhs_blank(self) -> None:
        ws = Workspace()
        ws.define('S1', Canvas.make_from('abc'), tags=[Lhs(), OldWorld()])
        ws.define('S2', Canvas.make_from('abd'), tags=[Rhs(), OldWorld()])
        ws.run_painter(OtherSide('S2', 'SS'))
        self.assertEqual(ws.eval('SS'), ws['S1'])

    def test_other_side_blank_rhs(self) -> None:
        ws = Workspace()
        ws.define('S1', Canvas.make_from('abc'), tags=[Lhs(), OldWorld()])
        ws.define('S2', Canvas.make_from('abd'), tags=[Rhs(), OldWorld()])
        ws.run_painter(OtherSide('SS', 'S2'))
        self.assertEqual(ws.eval('SS'), ws['S1'])

    def test_other_side2(self) -> None:
        ws = Workspace()
        ws.define('S1', Canvas.make_from('abc'), tags=[Lhs(), OldWorld()])
        ws.define('S2', Canvas.make_from('wrong'))
        ws.define('S3', Canvas.make_from('wrong'), tags=OldWorld())
        ws.define('SR', Canvas.make_from('abd'), tags=[Rhs(), OldWorld()])
        ws.run_painter(OtherSide('S1', 'SS'))
        self.assertEqual(ws.eval('SS'), ws['SR'])

    def test_other_side_two_worlds(self) -> None:
        ws = Workspace()
        ws.define('S1', Canvas.make_from('abc'), tags=[Lhs(), OldWorld()])
        ws.define('S2', Canvas.make_from('abd'), tags=[Rhs(), OldWorld()])
        ws.define('S3', Canvas.make_from('ijk'), tags=[Lhs(), NewWorld()])
        ws.define('S4', Canvas.make_from('ijl'), tags=[Rhs(), NewWorld()])
        ws.run_painter(OtherSide('S3', 'SS'))
        self.assertEqual(ws.eval('SS'), ws['S4'])
        ws.run_painter(OtherSide('S1', 'ST'))
        self.assertEqual(ws.eval('ST'), ws['S2'])

class TestPainterCluster(unittest.TestCase):

    def test_painter_cluster_just_define(self) -> None:
        ws = Workspace()
        ws.define('CLUSTER', PainterCluster(
            Define('LL', 'a')
        ))
        ws.run_painter_cluster('CLUSTER', empty_subst)
        self.assertEqual(ws['L1'], 'a')

    def test_painter_cluster_just_define_after_existing_letter(self) -> None:
        ws = Workspace()
        ws.define('L1', 'b')
        ws.define('CLUSTER', PainterCluster(
            Define('LL', 'a')
        ))
        ws.run_painter_cluster('CLUSTER', empty_subst)
        self.assertEqual(ws.all_letter_defs(), {'L1': 'b', 'L2': 'a'})

    def test_painter_cluster_assign_to_existing_letter(self) -> None:
        ws = Workspace()
        ws.define('L1', 'a')
        ws.define('CLUSTER', PainterCluster(
            Define('LL', 'a')
        ))
        ws.run_painter_cluster('CLUSTER', Subst.from_kwargs(LL='L1'))
        # Running the PainterCluster should do nothing, since L1 already exists
        # and is 'a'.
        self.assertEqual(ws.all_letter_defs(), {'L1': 'a'})

    def test_painter_cluster_local_same_name_as_existing_variable(self) -> None:
        ws = Workspace()
        ws.define('LL', 'a')
        ws.define('CLUSTER', PainterCluster(
            Define('LL', 'b')
        ))
        ws.run_painter_cluster('CLUSTER', empty_subst)
        self.assertEqual(ws.all_letter_defs(), {'LL': 'a', 'L1': 'b'})

    def test_painter_cluster_create_seed(self) -> None:
        ws = Workspace()
        ws.define('CLUSTER', PainterCluster(
            Define('DD', Seed('LL', 'II'))
        ))
        ws.run_painter_cluster('CLUSTER', Subst.from_kwargs(LL='a', II=1))
        self.assertEqual(ws.all_seed_defs(), {'D1': Seed('L1', 'I1')})
        self.assertEqual(ws.all_letter_defs(), {'L1': 'a'})
        self.assertEqual(ws.all_index_defs(), {'I1': 1})

    def test_painter_cluster_create_seed2(self) -> None:
        ws = Workspace()
        ws.define('L1', 'a')
        ws.define('I1', 1)
        ws.define('CLUSTER', PainterCluster(
            Define('DD', Seed('LL', 'II'))
        ))
        ws.run_painter_cluster('CLUSTER', Subst.from_kwargs(LL='L1', II='I1'))
        self.assertEqual(ws.all_seed_defs(), {'D1': Seed('L1', 'I1')})

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

        ws.run_painter_cluster(
            'CLUSTER',
            Subst.from_kwargs(SS='S1', DD='D1', FF=Succ)
        )
        self.assertEqual(ws['R1'], Repeat('S1', 'D1', 'F1'))
        self.assertEqual(ws['F1'], Succ)
        self.assertIsNone(ws['F2'])  # regression test: no duplicate Succ
        ws.run_repeater('R1')
        self.assertEqual(str(ws['S1']), 'abc')

        self.assertEqual(ws['CLUSTER'], PainterCluster(
            Define('RR', Repeat('SS', 'DD', 'FF'))
        ))

    #TODO A failed PainterCluster: ws should be unmodified--no half-done
    #entering of newly created objects.

    #TODO Nested PainterClusters.

    #TODO Fill a cluster's variables "bottom-up"

    def test_cluster_with_one_common_undefined_variable(self) -> None:
        ws = Workspace()
        ws.define('CLUSTER', PainterCluster(
            Define('DD1', Seed('LL1', 'II')),
            Define('LL1', 'a'),
            Define('DD2', Seed('LL2', 'II')),
            Define('LL2', 'i')
            # II is not defined: it will be filled in to be the same in both
            # seeds.
        ))

        ws.define('D1', Seed('L1', 'I1'))
        ws.define('L1', 'a')
        ws.define('I1', 1)
        ws.run_painter_cluster('CLUSTER', Subst.from_kwargs(DD1='D1'))

        self.assertEqual(ws['D2'], Seed('L2', 'I1'))
        self.assertEqual(ws.eval('D2'), Seed('i', 1))
        self.assertEqual(ws['L2'], 'i')

    def test_elems_out_of_order(self) -> None:
        ws = Workspace()
        ws.define('S1', Canvas.make_from('abc'), tags=[Lhs(), OldWorld()])
        ws.define('S2', Canvas.make_from('abd'), tags=[Rhs(), OldWorld()])
        ws.define('R1', Repeat('S1', Seed('a', 1), Succ))
        #ws.define('R2', Repeat('S2', Seed('a', 1), Succ))
        ws.define('CLUSTER', PainterCluster(
            OtherSide('SS1', 'SS2'), # SS1, SS2 occurs before either has a value
            Define('RR1', Repeat('SS1', 'DD', 'FF')),
            Define('RR2', Repeat('SS2', 'DD', 'FF')),
        ))
        ws.run_painter_cluster('CLUSTER', Subst.from_kwargs(RR1='R1'))
        self.assertEqual(ws['R2'], Repeat('S2', 'D1', 'F1'))


class TestArrow(unittest.TestCase):

    def setUp(self) -> None:
        self.ws = Workspace()
        self.ws.define('ARROW', PainterCluster(
            Define('RR1', Repeat('SS1', 'DD', 'FF')),
            Define('RR2', Repeat('SS2', 'DD', 'FF', 'EE')),
            OtherSide('SS1', 'SS2'),
            Define('EE', Skip('II')),
            Define('II', 3)
        ))

    def test_arrow(self) -> None:
        #self.ws.define('S1', Canvas.make_from('abc'), tags=[Lhs(), OldWorld()])
        #self.ws.define('S2', Canvas.make_from('abd'), tags=[Rhs(), OldWorld()])
        self.ws.define('S3', Canvas.make_from('ijk'), tags=[Lhs(), NewWorld()])
        self.ws.define('S4', Canvas.make_unknown(length=3), tags=[Rhs(), NewWorld()])
        self.ws.define('R1', Repeat('S3', Seed('i', 1), Succ))
        self.ws.run_painter_cluster('ARROW', Subst.from_kwargs(RR1='R1'))
        self.assertEqual(self.ws['R2'], Repeat('S4', 'D1', 'F1', 'E1'))
        #lo('R2', self.ws['R2'])
        #lo('R2EVAL', self.ws.eval('R2'))

        self.ws.run_repeater('R2')
        self.assertEqual(str(self.ws['S4']), 'ijl')

    # TODO Test with Canvas.make_unknown(), i.e. no length

class TestDiffer(unittest.TestCase):

    def test_diff_abc_abd(self) -> None:
        ws = Workspace()
        ws.define('S1', Canvas.make_from('abc'), tags=[Lhs(), OldWorld()])
        ws.define('R1', Repeat('S1', Seed('a', 1), Succ))
        ws.define('S2', Canvas.make_from('abd'), tags=[Rhs(), OldWorld()])
        ws.define('R2', Repeat('S2', Seed('a', 1), Succ, exception=Skip(3)))

        arrow = ws.construct_diff('R1', 'R2', name='ARROW')
        self.assertEqual(
            arrow,
            PainterCluster(
                Define('RR1', Repeat('SS1', 'DD', 'FF')),
                Define('RR2', Repeat('SS2', 'DD', 'FF', 'EE1')),
                OtherSide('SS1', 'SS2'),
                Define('EE1', Skip('II1')),
                Define('II1', 3)
            )
        )

#    def test_diff_abc_ijk(self) -> None:
#        ws = Workspace()
#        ws.define('S1', Canvas.make_from('abc'))
#        ws.define('R1', Repeat('S1', Seed('a', 1), Succ))
#        ws.define('S2', Canvas.make_from('ijk'))
#        ws.define('R2', Repeat('S2', Seed('i', 1), Succ))
#
#        arrow = ws.construct_diff('R1', 'R2', name='ARROW')
#        self.assertEqual(
#            arrow,
#            PainterCluster(
#                Define('RR1', Repeat('SS1', 'DD1', 'FF')),
#                Define('RR2', Repeat('SS2', 'DD2', 'FF')),
#                OtherSide('SS1', 'SS2'),
#                Define('DD1', Seed('LL1', 'II')),
#                Define('DD2', Seed('LL2', 'II')),
#                Define('LL1', 'a'),
#                Define('LL2', 'i'),
#            )
#        )
        # NOTE for dissertation: it's a shame that the difference between
        # 'abc' and 'ijk' isn't simply "they start on a different letter"
        # rather than a PainterCluster with seven elements. It would be better
        # if the simple difference was stored somehow, from which the
        # PainterCluster could be reconstructed as needed. Then the model
        # could compare simple differences.

    '''
    To diff repeaters, we will need a function that given an argument from
    one repeater and the corresponding argument from the other repeater,
    returns what needs to go into the PainterCluster.

    If the arguments are the same:
        a single name and no Define.
    If the arguments are (arbitrarily) different:
        two names, and two Defines -- plus any further names and Defines
        for parameters of the arguments (if they're CompoundWorkspaceObjs)
    If the arguments have some other relation between them:
        two names, and a Painter relating them
    '''

#    # TODO test case with multiple LLn's
#    def test_multiple_LLns(self) -> None:
#        ws = Workspace()
#        ws.define('L1', 'a')
#        ws.define('L2', 'a')
#        ws.define('L3', 'b')
#        ws.define('L4', 'b')
#        ws.define('D1', Seed('L1', 1))
#        ws.define('D2', Seed('L2', 1))
#        ws.define('D3', Seed('L3', 1))
#        ws.define('D4', Seed('L4', 1))

#    def test_diff_identical_letters(self) -> None:
#        ws = Workspace()
#        ws.define('L1', 'a')
#        ws.define('L2', 'a')
#        diff = ws.construct_diff('L1', 'L2', name='DIFF')
#        self.assertEqual(
#            diff,
#            Same('L1', 'L2')
##            PainterCluster(
##                Define('LL1', 'LL'),
##                Define('LL2', 'LL')
##            )
#        )

#    def test_diff_different_letters(self) -> None:
#        ws = Workspace()
#        ws.define('L1', 'a')
#        ws.define('L2', 'i')
#        diff = ws.construct_diff('L1', 'L2', name='DIFF')
#        self.assertEqual(
#            diff,
#            PainterCluster(
#                Define('LL1', 'a'),
#                Define('LL2', 'i')
#            )
#        )

#    def test_diff_succ_letter(self) -> None:
#        ws = Workspace()
#        ws.define('L1', 'a')
#        ws.define('L2', 'b')
#        diff = ws.construct_diff('L1', 'L2', name='DIFF')
#        self.assertEqual(
#            diff,
#            Succ('L1', 'L2')
#        )

#    def test_diff_identical_indices(self) -> None:
#        ws = Workspace()
#        ws.define('I1', 1)
#        ws.define('I2', 1)
#        diff = ws.construct_diff('I1', 'I2', name='DIFF')
#        self.assertEqual(
#            diff,
#            PainterCluster(
#                Define('II1', 'II'),
#                Define('II2', 'II')
#            )
#        )
#
#    def test_diff_different_indices(self) -> None:
#        ws = Workspace()
#        ws.define('I1', 1)
#        ws.define('I2', 3)
#        diff = ws.construct_diff('I1', 'I2', name='DIFF')
#        self.assertEqual(
#            diff,
#            PainterCluster(
#                Define('II1', 1),
#                Define('II2', 3)
#            )
#        )
#
#
#    # TODO diff Succ, Prev letters
#    # TODO diff Succ, Prev indices
#    # TODO diff Succ, Prev canvas lengths
#
#    def test_diff_different_seeds(self) -> None:
#        ws = Workspace()
#        ws.define('D1', Seed('L1', 'I1'))
#        ws.define('D2', Seed('L2', 'I2'))
#        ws.define('L1', 'a')
#        ws.define('L2', 'i')
#        ws.define('I1', 1)
#        ws.define('I2', 1)
#        diff = ws.construct_diff('D1', 'D2', name='DIFF')
#        self.assertEqual(
#            diff,
#            PainterCluster(
#                Define('DD1', Seed('LL1', 'II')),
#                Define('DD2', Seed('LL2', 'II')),
#                Define('LL1', 'a'),
#                Define('LL2', 'i')
#            )
#        )

    def test_diffcontext_diff_same_letter(self) -> None:
        ws = Workspace()
        ws.define('L1', 'a')
        ws.define('L2', 'a')
        context = DiffContext(ws)
        var1, var2 = context.add_diff('L1', 'L2')
        self.assertEqual(var1, 'LL')
        self.assertEqual(var2, 'LL')
        self.assertEqual(context.d['LL'], NoValue())

    def test_diffcontext_diff_different_letter(self) -> None:
        ws = Workspace()
        ws.define('L1', 'a')
        ws.define('L2', 'i')
        context = DiffContext(ws)
        var1, var2 = context.add_diff('L1', 'L2')
        self.assertEqual(var1, 'LL1')
        self.assertEqual(var2, 'LL2')
        self.assertEqual(context.d[var1], 'a')
        self.assertEqual(context.d[var2], 'i')

    def test_diffcontext_diff_same_seed(self) -> None:
        ws = Workspace()
        ws.define('D1', Seed('L1', 'I1'))
        ws.define('D2', Seed('L2', 'I2'))
        ws.define('L1', 'a')
        ws.define('L2', 'a')
        ws.define('I1', 1)
        ws.define('I2', 1)
        context = DiffContext(ws)
        var1, var2 = context.add_diff('D1', 'D2')
        self.assertEqual(var1, 'DD')
        self.assertEqual(var2, 'DD')
        self.assertEqual(context.d, {'DD': NoValue()})

    def test_diffcontext_diff_different_seeds(self) -> None:
        ws = Workspace()
        ws.define('D1', Seed('L1', 'I1'))
        ws.define('D2', Seed('L2', 'I2'))
        ws.define('L1', 'a')
        ws.define('L2', 'i')
        ws.define('I1', 1)
        ws.define('I2', 1)
        context = DiffContext(ws)
        var1, var2 = context.add_diff('D1', 'D2')
        self.assertEqual(var1, 'DD1')
        self.assertEqual(var2, 'DD2')
        self.assertEqual(context.d, {
            'DD1': Seed('LL1', 'II'),
            'DD2': Seed('LL2', 'II'),
            'LL1': 'a',
            'LL2': 'i',
            'II': NoValue()
        })

    def test_diffcontext_otherside(self) -> None:
        ws = Workspace()
        canvas1 = Canvas.make_from('abc')
        ws.define('S1', canvas1, tags=[Lhs(), OldWorld()])
        canvas2 = Canvas.make_from('ijk')
        ws.define('S2', canvas2, tags=[Rhs(), OldWorld()])
        context = DiffContext(ws)
        var1, var2 = context.add_diff('S1', 'S2')
        self.assertEqual(var1, 'SS1')
        self.assertEqual(var2, 'SS2')
        self.assertEqual(context.d, {
            'SS1': NoValue(),
            'SS2': NoValue(),
            'PP1': OtherSide('SS1', 'SS2')
        })

#    def test_diffcontext_otherside_different_worlds(self) -> None:
#        ws = Workspace()
#        canvas1 = Canvas.make_from('abc')
#        ws.define('S1', canvas1, tags=[Lhs(), OldWorld()])
#        canvas2 = Canvas.make_from('ijk')
#        ws.define('S2', canvas2, tags=[Rhs(), NewWorld()])
#        context = DiffContext(ws)
#        var1, var2 = context.add_diff('S1', 'S2')
#        #TODO Perhaps the Differ should fail or just treat these canvases
#        # as arbitrary. Not needed to get to 'ijl'.

    def test_diffcontext_repeater(self) -> None:
        ws = Workspace()
        ws.define('S1', Canvas.make_from('abc'), tags=[Lhs(), OldWorld()])
        ws.define('R1', Repeat('S1', Seed('a', 1), Succ))
        ws.define('S2', Canvas.make_from('ijk'), tags=[Rhs(), OldWorld()])
        ws.define('R2', Repeat('S2', Seed('i', 1), Succ))
        context = DiffContext(ws)
        var1, var2 = context.add_diff('R1', 'R2')
        self.assertEqual(var1, 'RR1')
        self.assertEqual(var2, 'RR2')
        self.assertEqual(context.d, {
            'RR1': Repeat('SS1', 'DD1', 'FF'),
            'RR2': Repeat('SS2', 'DD2', 'FF'),
            'SS1': NoValue(),
            'SS2': NoValue(),
            'PP1': OtherSide('SS1', 'SS2'),
            'DD1': Seed('LL1', 'II'),
            'DD2': Seed('LL2', 'II'),
            'LL1': 'a',
            'LL2': 'i',
            'II': NoValue(),
            'FF': NoValue()
        })

    def test_diffcontext_repeater_with_exception(self) -> None:
        ws = Workspace()
        ws.define('S1', Canvas.make_from('abc'), tags=[Lhs(), OldWorld()])
        ws.define('R1', Repeat('S1', Seed('a', 1), Succ))
        ws.define('S2', Canvas.make_from('abc'), tags=[Rhs(), OldWorld()])
        ws.define('R2', Repeat('S2', Seed('a', 1), Succ, Skip(3)))
        context = DiffContext(ws)
        var1, var2 = context.add_diff('R1', 'R2')
        self.assertEqual(var1, 'RR1')
        self.assertEqual(var2, 'RR2')
        self.assertEqual(context.d, {
            'RR1': Repeat('SS1', 'DD', 'FF'),
            'RR2': Repeat('SS2', 'DD', 'FF', 'EE1'),
            'SS1': NoValue(),
            'SS2': NoValue(),
            'PP1': OtherSide('SS1', 'SS2'),
            'DD': NoValue(),
            'II1': 3,
            'FF': NoValue(),
            'EE1': Skip('II1')
        })


    # TODO test DiffContext where one Repeat has an exception and the
    # other doesn't.

    # TODO test two references to same snippet: we should create only one
    # variable for both, not two variables


#    def test_diff_opposite_side_canvases(self) -> None:
#        pass #TODO

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

    def test_unify_DD1_D1(self) -> None:
        su = Subst()
        su = su.unify('DD1', Seed('LL1', 'II'))
        su = su.unify('D1', Seed('L1', 'I1'))
        su = su.unify('DD1', 'D1')
        #self.assertTrue(su.are_equal('LL1', 'L1'))
        #self.assertTrue(su.are_equal('II', 'I1'))
        #self.assertTrue(su.are_equal('DD1', 'D1'))
        su1 = su.unify('LL1', 'a').unify('I1', 1)
        self.assertEqual(su1.eval('D1'), Seed('a', 1))
        self.assertEqual(su1.eval('DD1'), Seed('a', 1))

    def test_unify_defined_variable_with_undefined_variable(self) -> None:
        su = Subst()
        su = su.unify('L1', 'a')
        su = su.unify('L1', 'L2')
        self.assertEqual(su.eval('L1'), 'a')
        self.assertEqual(su.eval('L2'), 'a')

    def test_unify_II_with_three_variables(self) -> None:
        su = Subst()
        su = su.unify('D1', Seed('L1', 'I1'))
        su = su.unify('D2', Seed('L2', 'I2'))
        su = su.unify('D3', Seed('L3', 'I3'))
        su = su.unify('II', 'I1')
        su = su.unify('II', 'I2')
        su = su.unify('II', 'I3')
#        self.assertTrue(su.are_equal('II', 'I1'))
#        self.assertTrue(su.are_equal('II', 'I2'))
#        self.assertTrue(su.are_equal('II', 'I3'))

        su1 = su.unify('I1', 1)
        self.assertEqual(su1.eval('I1'), 1)
        self.assertEqual(su1.eval('I2'), 1)
        self.assertEqual(su1.eval('I3'), 1)
        self.assertEqual(su1.eval('II'), 1)

        su2 = su.unify('I2', 1)
        self.assertEqual(su2.eval('I1'), 1)
        self.assertEqual(su2.eval('I2'), 1)
        self.assertEqual(su2.eval('I3'), 1)
        self.assertEqual(su2.eval('II'), 1)

        su3 = su.unify('I3', 1)
        self.assertEqual(su3.eval('I1'), 1)
        self.assertEqual(su3.eval('I2'), 1)
        self.assertEqual(su3.eval('I3'), 1)
        self.assertEqual(su3.eval('II'), 1)

    def test_unify_constant_with_compound_object(self) -> None:
        su = Subst()
        su = su.unify('a', Seed('a', 1))
        self.assertTrue(su.is_bottom())

    def test_unify_compound_object_with_constant(self) -> None:
        su = Subst()
        su = su.unify(Seed('a', 1), 'a')
        self.assertTrue(su.is_bottom())

    def test_unify_compound_object_with_variable(self) -> None:
        su = Subst()
        su = su.unify(Seed('a', 1), 'D1')
        self.assertEqual(su.eval('D1'), Seed('a', 1))

    def test_unify_DD1_DD2(self) -> None:
        su = Subst()
        # as in a PainterCluster:
        su = su.unify('DD1', Seed('LL1', 'II'))
        su = su.unify('LL1', 'a')
        su = su.unify('DD2', Seed('LL2', 'II'))
        su = su.unify('LL2', 'i')
        # as in the Workspace:
        su = su.unify('D1', Seed('L1', 'I1'))
        su = su.unify('L1', 'a')
        su = su.unify('I1', 1)
        # now "call" the PainterCluster:
        su = su.unify('DD1', 'D1')
        self.assertEqual(su.eval('DD2'), Seed('i', 1))

    def test_occurs_check_compound_object(self) -> None:
        su = Subst()
        su = su.unify('D1', Seed('D1', 1))  # circular reference
        self.assertTrue(su.is_bottom())

    def test_remove_variable(self) -> None:
        su = Subst()
        su = su.unify('L1', 'a')
        su = su.unify('L2', 'L1')
        su = su.remove('L2')
        self.assertEqual(su, Subst.from_kwargs(L1='a'))

    def test_remove_intervening_variable(self) -> None:
        su = Subst()
        su = su.unify('L1', 'a')
        su = su.unify('L2', 'L1')
        su = su.remove('L1')
        self.assertEqual(su, Subst.from_kwargs(L2='a'))

    def test_remove_nonexistent_variable(self) -> None:
        su = Subst()
        su = su.remove('L1')
        self.assertEqual(su, empty_subst)

    def test_remove_undefined_variable_that_is_referenced(self) -> None:
        # The references to an undefined variable should continue to exist after
        # the variable is removed. In other words, removing an undefined variable
        # has no effect.
        su = Subst()
        su = su.unify('D1', Seed('L1', 'I1'))
        su = su.remove('L1')
        self.assertEqual(su, Subst.from_kwargs(D1=Seed('L1', 'I1')))

    def test_remove_variable_defined_as_object_inside_compound_object(self) -> None:
        su = Subst()
        su = su.unify('D1', Seed('L1', 'I1'))
        su = su.unify('L1', 'a')  # "direct" definition of L1
        su = su.remove('L1')
        self.assertEqual(su, Subst.from_kwargs(D1=Seed('a', 'I1')))
        # Is this a bad idea--leaving 'a' (a constant) inside the Seed?
        # Shouldn't we create a new variable to hold the 'a'?

    def test_remove_indirectly_defined_variable_inside_compound_object(self) -> None:
        su = Subst()
        su = su.unify('D1', Seed('LL', 'I1'))
        su = su.unify('LL', 'L1')  # "indirect" definition of LL
        su = su.unify('L1', 'a')
        su = su.remove('LL')
        self.assertEqual(su, Subst.from_kwargs(
            D1=Seed('L1', 'I1'),
            L1='a'
        ))



    # TODO? occurs-check for indirect circular reference, like D1=Seed(E1, 1),
    # E1=D1

    # TODO Creating a new object -- keep this separate from unification

    # TODO test that on exit from a PainterCluster, all local variables are
    # eliminated, all new objects are created and named, and all 'same' relations
    # between variables in the new objects are preserved.


class TestLengthPainter(unittest.TestCase):
    
    def test_run_length_painter_right(self) -> None:
        ws = Workspace()
        ws.define('S1', Canvas.make_from('abc'))
        ws.define('S2', Canvas.make_unknown())
        ws.define('P1', LengthPainter('S1', 'S2', Same))
        assert ws.get_canvas('S2').length is None
        ws.run_painter('P1')
        self.assertEqual(ws.get_canvas('S2').length, 3)

    def test_run_length_painter_left(self) -> None:
        ws = Workspace()
        ws.define('S1', Canvas.make_unknown())
        ws.define('S2', Canvas.make_from('abc'))
        ws.define('P1', LengthPainter('S1', 'S2', Same))
        assert ws.get_canvas('S1').length is None
        ws.run_painter('P1')
        self.assertEqual(ws.get_canvas('S1').length, 3)

    # TODO Test case with one canvas missing
    # TODO Test case with both canvases missing

    def test_run_length_painter_conflicting_lengths(self) -> None:
        ws = Workspace()
        ws.define('S1', Canvas.make_from('abc'))
        ws.define('S2', Canvas.make_from('abcd'))
        ws.define('P1', LengthPainter('S1', 'S2', Same))
        with self.assertRaises(ArgumentsFailRelation):
            ws.run_painter('P1')

    def test_run_length_painter_same_lengths(self) -> None:
        ws = Workspace()
        ws.define('S1', Canvas.make_from('abc'))
        ws.define('S2', Canvas.make_from('abc'))
        ws.define('P1', LengthPainter('S1', 'S2', Same))
        ws.run_painter('P1')
        # If no exception, then we succeeded. Since both canvases have the
        # same length, the LengthPainter has nothing to do.

    # TODO Test case for canvases where length increases by 1
    # TODO Test case for canvases where length decreases by 1
    # TODO Test case for canvases where length changes by more than 1
