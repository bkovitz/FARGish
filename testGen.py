# testGen.py -- Tests of code-generation classes in gen.py

import unittest
from io import StringIO
from pprint import pprint as pp

from gen import gen, IfStmt, NullStmt
from Env import Env
from Indenting import Indenting


def test_gen(o, env):
    file = Indenting(StringIO())
    fixup = Indenting(StringIO())
    gen(o, file, fixup, env)
    fixup.seek(0)
    for line in fixup:
        print(line, file=file, end='')
    return file.getvalue()

class TestGen(unittest.TestCase):

    def test_ifstmt(self):
        env = Env()
        ifstmt = IfStmt(None, NullStmt(), None)
        #DEBUG
        print()
        print(test_gen(ifstmt, env))

