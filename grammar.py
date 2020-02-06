# grammar.py -- The grammar for FARGish
#
# Written in PLY (Python Lex Yacc): https://www.dabeaz.com/ply/

import ply.lex as lex
import ply.yacc as yacc

from Indent1 import Parser
from util import nice_object_repr


# Grammar for lexical analyzer

tokens = (
    'NAME',
    'LPAREN',
    'RPAREN',
    'INDENT',
    'DEDENT',
    'WS', # white space
    'NEWLINE',
    'ENDMARKER'
)

literals = ',:'

def t_LPAREN(t):
    r'\('
    t.lexer.paren_count += 1
    return t

def t_RPAREN(t):
    r'\)'
    # check for underflow?  should be the job of the parser
    t.lexer.paren_count -= 1
    return t

KEYWORDS = {}

def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = KEYWORDS.get(t.value, 'NAME')
    return t

# t_comment appears ahead of t_WS so that t_comment consumes entire lines
# containing only comments, so t_WS never sees them. t_comment does not
# consume the newline.
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

# Grammar for syntactic analyzer

class NodeDef:

    def __init__(self, name, ancestor_names=None):
        self.name = name
        if ancestor_names is None:
            ancestor_names = []
        self.ancestor_names = ancestor_names

    __repr__ = nice_object_repr

class NameWithArguments:

    def __init__(self, name, arguments):
        self.name = name
        self.arguments = arguments

    __repr__ = nice_object_repr

def p_prog(p):
    """prog : empty
            | prog nodedef"""
    if len(p) == 2:  # if empty
        p[0] = []
    else:
        p[0] = p[1]
        if isinstance(p[2], list):
            p[0] += p[2]
        else:
            p[0].append(p[2])

def p_nodedef(p):
    """nodedef : nodenames maybe_ancestors"""
    p[0] = [NodeDef(name, p[2]) for name in p[1]]

def p_nodenames(p):
    """nodenames : nodename
                 | nodenames ',' nodename"""
    if len(p) == 2:  # if first NAME
        p[0] = [p[1]]
    else:
        p[0] = p[1]
        p[0].append(p[3])

def p_nodename(p):
    """nodename : NAME
                | NAME LPAREN names RPAREN"""
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = NameWithArguments(p[1], p[3])

def p_names(p):
    """names : NAME
             | names ',' NAME"""
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1]
        p[0].append(p[3])

def p_maybe_ancestors(p):
    """maybe_ancestors : empty
                       | ':' nodenames"""
    if len(p) == 2:  # if empty
        p[0] = []
    else:
        p[0] = p[2]

def p_empty(p):
    """empty :"""
    pass



parser = Parser(lex.lex(), yacc.yacc(debug=5))

prog = """
Number(n)

Brick(x), Block : Number

Target
"""
got = parser.parse(prog)
print(got)
