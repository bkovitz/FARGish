# codegen.py -- Code generation: FARGish -> Python

import sys
from io import StringIO
from pprint import pprint as pp

from grammar import parse
from gen import LinkDefn
from Env import Env
from Indenting import Indenting, indent
#from PortGraph import Node


preamble = '''from PortGraph import Node
from LinkSpec import LinkSpec
from NodeSpec import NodeOfClass, NodeSpec, NodeWithTag, \
    NodeWithValue, CartesianProduct, no_dups, TupAnd, NotLinkedToSame, \
    NotAlreadyBuilt
from bases import ActiveNode
from NodeParams import NodeParams, MateParam, AttrParam
from PortMates import PortMates
from Action import Build3, make_build3
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

    #TODO This really should become a special item in Env, so that .gen code
    #can derive things from it.
    # 'port_mates' is global variable. You must pass it explicitly to
    # PortGraph's ctor.
    print('port_mates = PortMates()', file=file)
    for link_defn in (i for i in items if isinstance(i, LinkDefn)):
        fl = repr(link_defn.from_label.name)
        tl = repr(link_defn.to_label.name)
        print(f"port_mates.add({fl}, {tl})", file=file)

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
