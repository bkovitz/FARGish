# grammar.py -- The grammar for FARGish
#
# Written in PLY (Python Lex Yacc): https://www.dabeaz.com/ply/

import ply.lex as lex
import ply.yacc as yacc

from util import nice_object_repr

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

def t_LPAREN(t):
    r'\('
    t.lexer.paren_count += 1
    return t

def t_RPAREN(t):
    r'\)'
    # check for underflow?  should be the job of the parser
    t.lexer.paren_count -= 1
    return t

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
