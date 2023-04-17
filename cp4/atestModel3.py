# atestModel3.py -- Acceptance tests for the "FakeIt" canvas-and-painters model
#                   as it grows into a real model

import unittest
import inspect
from pprint import pp
if 'unittest.util' in __import__('sys').modules:
    # Show full diff in self.assertEqual.
    __import__('sys').modules['unittest.util']._MAX_LENGTH = 999999999

from Model3 import at, run, At, Detected, Succ, MadePainter, PainterCluster, \
    CanvasAddress, Define, RanPainter


class TestModel3(unittest.TestCase):

    def test_ab_(self) -> None:
        m = run('ab_')
        self.assertEqual(
            m.events,
            [ 
                Detected(Succ(at('C1', 1), at('C1', 2))),
                MadePainter('P1', Succ(at('C1', 1), at('C1', 2))),
                Detected(Succ(at('P1', 'left'), at('P1', 'right'))),
                MadePainter('P2',
                    PainterCluster(
                        Define('PP1', Succ(At('AA1'), At('AA2'))),
                        Define('AA1', CanvasAddress('SS', 'II1')),
                        Define('AA2', CanvasAddress('SS', 'II2')),
                        Succ('II1', 'II2')
                    )
                ),
                RanPainter('P2', AA1=CanvasAddress('C1', 2)),
                MadePainter('P3',
                    Succ(At(CanvasAddress('C1', 2)), At(CanvasAddress('C1', 3)))
                ),
                RanPainter('P3')
            ]
        )
        """
            [
                #Detected(Succ(At(CanvasAddress('C1', 1)), At(CanvasAddress('C1', 2))))),
                Detected(Succ(at('C1', 1), at('C1', 2))),
                MadePainter(Succ(At('C1', 1), At('C1', 2)),
                Detected(Succ(At('P1', 'left'), At('P1', 'right')),
                MadePainter('P2',
                    PainterCluster(
                        Define('PP1', Succ(At('AA1'), At('AA2'))),
                        Define('AA1', Address('SS', 'II1')),
                        Define('AA2', Address('SS', 'II2')),
                        Succ('II1', 'II2')
                    )
                )
                RanPainter('P2', AA1=Address('C1', 2)),
                MadePainter('P3',
                    Succ(At(Address('C1', 2)), At(Address('C1', 3)))
                ),
                RanPainter('P3')

#                Detected(SuccAt(Address('C1', 1), Address('C1', 2))),
#                MadePainter(SuccAt(Address('C1', 1), Address('C1', 2)),
#                Detected(SuccAt('P1', 'left'), Address('P1', 'right')),
#                MadePainter('P2',
#                    PainterCluster(
#                        Define('PP1', SuccAt('AA1', 'AA2')),
#                        Define('AA1', Address('SS', 'II1')),
#                        Define('AA2', Address('SS', 'II2')),
#                        Succ('II1', 'II2')
#                    )
#                )
#                RanPainter('P2', AA1=Address('C1', 2)),
#                MadePainter('P3', SuccAt(Address('C1', 2), Address('C1', 3))),
#                RanPainter('P3')
            ]
        """
        self.assertEqual(m.solution(), 'abc')
        # run Succ detector on 'ab_', get Succ((C1, 1), (C1, 2))
        # run ArgumentRelationDetector on that Succ, get PainterCluster
        # run PainterCluster on AA1=(C1, 2), get Succ((C1, 2), (C1, 3))
        # run that Succ, get a new canvas: 'abc'

