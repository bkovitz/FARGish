# codegen.py -- Code generation: FARGish -> Python

import sys
from io import StringIO
from pprint import pprint as pp

from grammar import parse
from gen import LinkDefn, PortLabelParent, ExtGFunc, ExtFunc, Postamble
from Env import Env
from Indenting import Indenting, indent
#from PortGraph import Node
from Predefs import Tagged, AllTagged, Not


preamble = '''from PortGraph import Node
from LinkSpec import LinkSpec
from NodeSpec import NodeOfClass, NodeSpec, NodeWithTag, \
    NodeWithValue, CartesianProduct, no_dups, TupAnd, NotLinkedToSame, \
    NotAlreadyBuilt, TupFunc
from ActiveNode import ActiveNode
from NodeParams import NodeParams, MateParam, AttrParam
from PortMates import PortMates
from Hierarchy import Hierarchy
from Action import Build3, make_build3
from Predefs import Tagged, AllTagged, Not
'''

predefs = [
    LinkDefn('agents', 'behalf_of'),
    LinkDefn('members', 'member_of'),
    ExtGFunc('Tagged'),
    ExtGFunc('AllTagged'),
    ExtFunc('Not')
]

def make_python(
    fargish_code,
    file=None,  # If None, print to stdout
    predefs=predefs,
    preamble=preamble,
    postamble='',
    debug=False  # If True, print the raw parsed items to stdout
):
    if file is None:
        file = sys.stdout
    file = Indenting(file)
    items = parse(fargish_code, predefs=predefs)
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
    # 'port_mates' is a global variable. You must pass it explicitly to
    # PortGraph's ctor.
    print('port_mates = PortMates()', file=file)
    for link_defn in (i for i in items if isinstance(i, LinkDefn)):
        fl = repr(link_defn.from_label.name)
        tl = repr(link_defn.to_label.name)
        print(f"port_mates.add({fl}, {tl})", file=file)

    print('port_hierarchy = Hierarchy()', file=file)
    for chpa in (i for i in items if isinstance(i, PortLabelParent)):
        ch = repr(chpa.child)
        pa = repr(chpa.parent)
        print(f'port_hierarchy.parent({pa}, {ch})', file=file)

    fixup.seek(0)
    for line in fixup:
        print(line, file=file, end='')

    for pa in (i for i in items if isinstance(i, Postamble)):
        pa.print(file)
    print(postamble, file=file, end='')

def compile_fargish(fargish_code, saveto=None):
    '''saveto is name of file to hold a copy of the generated Python code,
    or None to not save it.'''
    s = StringIO()
    make_python(fargish_code, file=s)
    filename = '<string>'
    if saveto:
        with open(saveto, 'w') as sf:
            sf.write(s.getvalue())
        filename = saveto
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
