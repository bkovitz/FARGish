# testCodegen.py -- Tests of the FARGish grammar and compilation to Python

import unittest

from codegen import make_python, compile_fargish

class TestCodegen(unittest.TestCase):

    def test1(self):
        prog = '''
SomeNode
'''
        got = make_python(prog)
        print('GOT', got)
