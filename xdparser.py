# xdparser.py -- Experiments to figure out how to use dparser

from dparser import Parser, Reject
from sys import stdout

from util import NiceRepr


space8 = ' ' * 8
newline = '\n'

class Nodeclass(NiceRepr):

    def __init__(self, name, args, parent_inits):
        self.name = name
        self.args = args
        self.parent_inits = parent_inits
        self.elems = []

    def add_elem(self, elem):
        self.elems.append(elem)

    def gen(self, file):
        if self.parent_inits:
            parents = f"({', '.join(i.name for i in self.parent_inits)})"
        else:
            parents = ''
        arg_asgns = '\n'.join(
            space8 + f"self.{arg} = {arg}" for arg in self.args
        )
        super_inits = '\n'.join(
            space8 + f"super({p.name}, self).__init__({p.argstring()})"
                for p in self.parent_inits
        )
        elem_stmts = '\n'.join(
            space8 + e.stmt() for e in self.elems
        )
        print(f'''
class {self.name}{parents}:
    def __init__({', '.join(['self'] + self.args)}):
{arg_asgns}
{super_inits}
{elem_stmts}''', file=file)

class NodeclassAttr(NiceRepr):

    def __init__(self, name, expr):
        self.name = name
        self.expr = expr

    def stmt(self):
        return f"self.{self.name} = {self.expr}"

class NodeclassInit(NiceRepr):
    '''A nodeclass name or a nodeclass name provided with arguments. This
    might be a declaration of a nodeclass or a reference to an already-
    defined nodeclass.'''

    def __init__(self, name, args=None):
        self.name = name
        if args is None:
            args = []
        self.args = args

    def argstring(self):
        return ', '.join(self.args)

class NodeclassHeading(NiceRepr):
    '''The first line of a nodeclass definition: one or more nodeclasses
    being defined (the lhs), and one or more nodeclasses that they all
    inherit from (the rhs).'''

    def __init__(self, lhs_inits, rhs_inits=None):
        self.lhs_inits = lhs_inits
        if rhs_inits is None:
            rhs_inits = []
        self.rhs_inits = rhs_inits


def generate_code(definitions, file=None):
    if isinstance(definitions, list):
        for definition in definitions:
            generate_code(definition, file)
    else:
        definitions.gen(file)

indent_level = 0  # TODO rm?
start_column = 0   # column of first nonwhitespace char on current line
indent_stack = []  # start_columns of previous indent levels

def d_fargspec(t):
    'fargspec : definition*'
    return t[0]

def d_definition(t):
    'definition : nodeclass_heading indent nodeclass_elem* outdent'
    defs = []
    for init in t[0].lhs_inits:
        nodeclass = Nodeclass(init.name, init.args, t[0].rhs_inits)
        for elem in t[2]:
            nodeclass.add_elem(elem)
        defs.append(nodeclass)
    return defs

def d_definition2(t):
    'definition : nodeclass_heading'
    defs = []
    for init in t[0].lhs_inits:
        nodeclass = Nodeclass(init.name, init.args, t[0].rhs_inits)
        defs.append(nodeclass)
    return defs

def d_nodeclass_heading1(t):
    r"nodeclass_heading : nodeclass_inits"
    return NodeclassHeading(t[0])

def d_nodeclass_heading2(t):
    r"nodeclass_heading : nodeclass_inits '\\' nodeclass_inits"
    return NodeclassHeading(t[0], t[2])

def d_nodeclass_inits(t):
    "nodeclass_inits : nodeclass_init (',' nodeclass_init)*"
    print('INITS', t)
    inits = [t[0]]
    for init in t[1]:
        inits.append(init[1]) # skip the comma
    return inits

def d_nodeclass_init(t):
    r"nodeclass_init : identifier | identifier '(' args ')'"
    #"nodeclass : identifier '(' (identifier (',' identifier)*)? ')'"
    return NodeclassInit(t[0], t[2])

#def d_nodeclass(t):
#    r"nodeclass : identifier | identifier '(' args ')'"
#    #"nodeclass : identifier '(' (identifier (',' identifier)*)? ')'"
#    return NodeclassInit(t[0], t[2])

def d_args(t):
    r"args : | identifier (',' identifier)*"
    args = []
    while t:
        args.append(t[0])
        t = t[1]
    return args

def d_identifier(t):
    r'identifier : "[A-Za-z_][A-Za-z0-9_]*"'
    return t[0]

def d_nodeclass_elem(t):
    "nodeclass_elem : identifier '=' expr"
    return NodeclassAttr(t[0], t[2])

def d_expr(t):
    "expr : identifier"
    return t[0]

    

# EXPERIMENTING WITH INDENT IN DPARSER

def d_whatever(t):
    "whatever : item*"
    print('WHATEVER', t)
    return t[0]

#def d_seq(t):
#    r'seq: indent item* outdent'
#    return t

def d_item(t):
    r'item : "[A-Za-z0-9+-*/()=\\]+"'
    print('ITEM', t)
    return t[0]

def d_punctuation_separator(t):
    r'item : ","'
    return t[0]

def d_wh(t):
    'item : " "'
    return t[0]

def d_newline(t):
    'item : "\n"'
    return t[0]

def d_item_indent(t):
    'item : indent'
    return t[0]

def d_item_outdent(t):
    'item: outdent'
    return t[0]

def d_indent(t, spec_only):
    'indent: "x"?'
    print('INDENT', indent_stack, start_column)
    del t, spec_only
    if not indent_stack or indent_stack[-1] < start_column:
        indent_stack.append(start_column)
        print('INDENTED')
        return 'indent'
    else:
        return Reject

def d_outdent(t, spec_only):
    'outdent: "x"?'
    del t, spec_only
    if indent_stack and indent_stack[-1] > start_column:
        #TODO Should somehow check that outdent goes to correct column
        indent_stack.pop()
        print('OUTDENTED')
        return 'outdent'
    else:
        return Reject

def whitespace(loc):
    global indent_level, start_column
    if loc.s < len(loc.buf):
        print('WHITESPACE', loc.s, len(loc.buf), repr(chr(loc.buf[loc.s])))
    else:
        print('WHITESPACE', loc.s, len(loc.buf))
    while loc.s < len(loc.buf):
        c = chr(loc.buf[loc.s])
        if c == '\n':
            start_column = 0
        elif c.isspace():
            start_column += 1
        #TODO Remove # to end of line
        else: # c is not whitespace, so start_column is new indent level
            return
        loc.s += 1

p = Parser()
print(dir(p))
p.inserted_thing = 'YAH'
#print(p.parse('2+3+4', initial_skip_space_fn=whitespace).getStructure())
#print()

s = open('numbo3.farg', 'r').read()
x = p.parse(s, initial_skip_space_fn=whitespace)
print(x.getStructure())
generate_code(x.getStructure())

#print()
#y = p.parse("   blah\nx\n", initial_skip_space_fn=whitespace)
