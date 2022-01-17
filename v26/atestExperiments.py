# atestExperiments.py -- Experiments to save and repeat for the dissertation

import unittest
from pprint import pprint as pp
import inspect

from Experiments import xp_single, just_1_1_2
from Log import lo, trace
from util import as_tuple, pr, pts, ps, pss, psa


class ATestExperiments(unittest.TestCase):

    def test_xp_single(self) -> None:
        # Verifies that an RMem trained on a single equation can regenerate
        # that equation reliably.
        rmem = just_1_1_2()
        expect = (1, '+', 1, '=', 2)
        for startc in [
            (1, None, None, None, None),
            (None, '+', 1, None, None),
            (None, None, None, None, 2)
        ]:
            got = [rmem.run_gset(canvas=startc) for _ in range(20)]
            self.assertTrue(
                all(canvas.as_tuple() == expect for canvas in got),
                f'Failed on {startc}'
            )
