# codegen.py -- Code generation: FARGish -> Python

import sys
from io import StringIO
from pprint import pprint as pp

from grammar import parse
from Env import Env
#from PortGraph import Node


preamble = '''from PortGraph import Node
from LinkSpec import LinkSpec
from NodeSpec import BuildSpec
from bases import ActiveNode
'''

def make_python(fargish_code, file=None, preamble=preamble, postamble=''):
    if file is None:
        file = sys.stdout
    items = parse(fargish_code)
    env = Env(items)
    print(preamble, file=file)
    fixup = StringIO()
    for item in items:
        if hasattr(item, 'gen'):
            item.gen(file, env, fixup)
    fixup.seek(0)
    for line in fixup:
        print(line, file=file, end='')
    print(postamble, file=file, end='')

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
