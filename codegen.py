# codegen.py -- Code generation: FARGish -> Python

import sys
from io import StringIO

from grammar import parse


def make_python(fargish_code, file=None):
    if file is None:
        file = sys.stdout
    items = parse(fargish_code)
    env = dict((o.name, o) for o in items)
    for item in items:
        item.gen(file, env)

def compile_fargish(fargish_code, filename='<string>'):
    s = StringIO()
    make_python(fargish_code, file=s)
    return compile(s.getvalue(), filename, 'exec')


prog = """
Number(n)
Brick : Number
"""
# c = compile_fargish(prog)
# exec(c)
make_python(prog)
