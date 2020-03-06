# codegen.py -- Code generation: FARGish -> Python

import sys
from io import StringIO
from pprint import pprint as pp

from grammar import parse
from Env import Env
from Indenting import Indenting, indent
#from PortGraph import Node


preamble = '''from PortGraph import Node
from LinkSpec import LinkSpec
from NodeSpec import BuildSpec
from bases import ActiveNode
from NodeParams import NodeParams, MateParam, AttrParam
'''

def make_python(
    fargish_code,
    file=None,  # If None, print to stdout
    preamble=preamble,
    postamble='',
    debug=False  # If True, print the raw parsed items to stdout
):
    if file is None:
        file = sys.stdout
    file = Indenting(file)
    items = parse(fargish_code)
    if (debug):
        print('\n')
        pp(items)
        print()
    env = Env(items)
    print(preamble, file=file)
    fixup = Indenting(StringIO())
    for item in items:
        if hasattr(item, 'gen'):
            item.gen(file, fixup, env)
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
