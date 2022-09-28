# testLog.py -- Unit tests for Log.py

import unittest
import inspect

from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from io import StringIO
import sys

from Log import log_to, lo, indent_log, set_log_level

class TestLog(unittest.TestCase):

    def setUp(self) -> None:
        set_log_level(0)

    def tearDown(self) -> None:
        set_log_level(0)
        log_to(sys.stdout)

    def test_lo(self) -> None:
        self.logged = StringIO()
        log_to(self.logged)
        set_log_level(2)

        lo('FIRST')
        self.assertEqual(self.logged_lines(), ['FIRST'])

        lo(3, 'SKIP1')
        self.assertEqual(self.logged_lines(), ['FIRST'])

        lo(2, 'SKIP1a')
        self.assertEqual(self.logged_lines(), ['FIRST', 'SKIP1a'])

        lo(1, 'SKIP1b')
        self.assertEqual(self.logged_lines(), ['FIRST', 'SKIP1a', 'SKIP1b'])

    def test_indent_log(self) -> None:
        self.logged = StringIO()
        log_to(self.logged)
        set_log_level(2)

        with indent_log():
            lo(2, 'FIRST')

            with indent_log(2, 'TASK X'):
                lo(3, 'SKIP')
                lo(2, 'SECOND')

            with indent_log(3, 'TASK Y'):
                lo(2, 'THIRD')

        self.assertEqual(
            self.logged_lines(),
            [
                '',
                '    FIRST',
                '    TASK X',
                '        SECOND',
                '        THIRD'
            ]
        )

    def logged_lines(self) -> List[str]:
        return [line.rstrip() for line in self.logged.getvalue().splitlines()]
