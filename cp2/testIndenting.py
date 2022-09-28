import unittest
from io import StringIO

from Indenting import Indenting, indent

class TestIndenting(unittest.TestCase):

    def test_indenting(self):
        sio = StringIO()
        f = Indenting(sio)
        self.assertFalse(f.wrote_any)
        print('Zero', file=f)
        self.assertTrue(f.wrote_any)
        with indent(f):
            print('Indented\nonce', file=f)
            with indent(f):
                print('Indented twice', file=f)
            print(file=f)
            print('Back to once\n', file=f)
        print('Back to nothing', file=f)

        expect = '''Zero
    Indented
    once
        Indented twice

    Back to once

Back to nothing
'''
        self.assertEqual(sio.getvalue(), expect)
        self.assertTrue(f.wrote_any)
