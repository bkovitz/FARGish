# codegen.py -- Code generation: FARGish -> Python

import sys
from io import StringIO
from pprint import pprint as pp

from grammar import parse
from Env import Env
from PortGraph import Node
from bases import NewLinkSpec


def make_python(fargish_code, file=None):
    if file is None:
        file = sys.stdout
    items = parse(fargish_code)
    pp(items)
    env = Env(items)
    for item in items:
        if hasattr(item, 'gen'):
            item.gen(file, env)

def compile_fargish(fargish_code, filename='<string>'):
    s = StringIO()
    make_python(fargish_code, file=s)
    return compile(s.getvalue(), filename, 'exec')


if __name__ == '__main__':
    prog = """
target -- tags

Number(n)
Brick : Number

OperandScout(target)
    """
    #c = compile_fargish(prog)
    #exec(c)
    make_python(prog)
