import unittest

import ply.lex as lex
import ply.yacc as yacc

from Indent1 import Parser
from util import nice_object_repr


# Simple grammar for lexical analyzer

tokens = (
    'NAME',
    'CLASS',
    'LPAREN',
    'RPAREN',
    'INDENT',  # Indent1 will output INDENT tokens
    'DEDENT',  # Indent1 will output DEDENT tokens
    'WS',      # Lexer must generate WS (whitespace) and NEWLINE tokens
    'NEWLINE'
)

def t_LPAREN(t):
    r'\('
    t.lexer.paren_count += 1
    return t

def t_RPAREN(t):
    r'\)'
    # check for underflow?  should be the job of the parser
    t.lexer.paren_count -= 1
    return t

KEYWORDS = {
    'class': 'CLASS'
}

def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = KEYWORDS.get(t.value, 'NAME')
    return t

# Putting this before t_WS lets it consume lines with only comments in
# them so the latter code never sees the WS part.  Not consuming the
# newline.  Needed for "if 1: #comment"

def t_comment(t):
    r"[ ]*\043[^\n]*"  # \043 is '#'
    pass

# Whitespace
def t_WS(t):
    r'[ ]+'
    if t.lexer.at_line_start and t.lexer.paren_count == 0:
        return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    t.type = 'NEWLINE'
    # Don't generate newline tokens when inside of parenthesis, eg
    #   a = (1,
    #        2, 3)
    if t.lexer.paren_count == 0:
        return t

def t_error(t):
    raise SyntaxError("Unknown symbol %r" % (t.value[0],))
    print("Skipping", repr(t.value[0]))
    t.lexer.skip(1)
# Simple grammar for syntactic analyzer

class ClassDef:
    '''A 'class' parsed from the tiny language. A class has a .name and
    zero or more arguments, stored in .elems.'''
    def __init__(self, name, elems=None):
        self.name = name
        if elems is None:
            elems = []
        self.elems = elems

    def __eq__(self, other):
        return (
            isinstance(other, self.__class__)
            and
            self.name == other.name
            and
            self.elems == other.elems
        )

    def __hash__(self):
        return hash(repr(self))

    __repr__ = nice_object_repr

def p_start(p):
    """start : prog """
    p[0] = p[1]

def p_prog(p):
    """prog : empty
            | prog classdef"""
    #print('PROG', len(p))
    if len(p) == 2:  # if empty
        p[0] = []
    else:
        p[0] = p[1]
        p[0].append(p[2])

def p_classdef(p):
    """classdef : CLASS NAME
                | CLASS NAME INDENT class_elems DEDENT"""
    #print('CLASSDEF', len(p))
    #for i in range(len(p)):
    #    print(i, p[i])
    if len(p) == 3:
        p[0] = ClassDef(p[2])
    else:
        p[0] = ClassDef(p[2], p[4])

def p_class_elems(p):
    """class_elems : empty
                   | class_elems class_elem"""
    if len(p) == 2:
        p[0] = []
    else:
        p[0] = p[1]
        p[0].append(p[2])

def p_class_elem(p):
    """class_elem : NAME
                  | LPAREN class_elems RPAREN"""
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[2]

def p_empty(p):
    """empty :"""
    pass

# Unit tests

parser = Parser(lex.lex(), yacc.yacc())

class TestIndent1(unittest.TestCase):

    def testSimplest(self):
        prog = """class Blah"""
        got = parser.parse(prog)
        expect = [ClassDef('Blah')]
        self.assertEqual(got, expect)

    def testIndent(self):
        prog = """
class Blah
    a
    b
class Gah
 c
    d
"""
        got = parser.parse(prog)
        expect = [ClassDef('Blah', ['a', 'b']), ClassDef('Gah', ['c', 'd'])]
        self.assertEqual(got, expect)

    def testParens(self):
        prog = """
class Blah
    (a
b)

class Gah

 (c
   d (e
f
  g)
  )

class Whah

"""
        got = parser.parse(prog)
        expect = [
            ClassDef(name='Blah', elems=[['a', 'b']]),
            ClassDef(name='Gah', elems=[['c', 'd', ['e', 'f', 'g']]]),
            ClassDef(name='Whah', elems=[])
        ]
        self.assertEqual(got, expect)
