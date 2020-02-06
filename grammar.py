# grammar.py -- The grammar for FARGish
#
# Written in PLY (Python Lex Yacc): https://www.dabeaz.com/ply/

import ply.lex as lex
import ply.yacc as yacc

tokens = (
    'NAME',
    'INDENT',
    'DEDENT',
    'WS', # white space
    'NEWLINE',
    'ENDMARKER'
)
