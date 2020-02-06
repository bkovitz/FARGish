# Indent1Starter.py -- A very simple language demonstrating how to parse a
#                      single level of syntactically significant indentation
#
# You should be able to copy and modify this to easily create a more complex
# language with a single indentation level.

import ply.lex as lex
import ply.yacc as yacc

from inspect import isclass

def nice_object_repr(self):
    '''Stick  __repr__ = nice_object_repr  inside a class definition and
    repr() will return a nice string for most classes.'''
    items = self.__dict__.items()
    if len(items) == 1:
        return '%s(%s)' % (self.__class__.__name__, next(iter(items))[1])
    elif len(items) == 0:
        return self.__class__.__name__
    else:
        return '%s(%s)' % (self.__class__.__name__,
                           ', '.join('%s=%s' % (k, nrepr(v))
                                       for k, v in items))

def nrepr(o):
    '''Helper for nice_object_repr().'''
    if isclass(o):
        return o.__name__
    elif isinstance(o, float):
        return '%.3f' % o
    else:
        return repr(o)

tokens = (
    'NAME',
    'CLASS',
    'LPAREN',
    'RPAREN',
    'INDENT',
    'DEDENT',
    'WS',  # white space
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

KEYWORDS = {
    'class': 'CLASS',
    'INDENT': 'INDENT',
    'DEDENT': 'DEDENT'
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

########################################################################
#
# The Indent1Lexer and its stream-filtering functions
#

class Indent1Lexer:

    def __init__(self, debug=0, optimize=0, lextab='lextab', reflags=0):
        self.lexer = lex.lex(debug=debug, optimize=optimize,
                             lextab=lextab, reflags=reflags)
        self.token_stream = None

    def input(self, s, add_endmarker=True):
        '''Sets character input stream to s.'''
        self.lexer.paren_count = 0
        self.lexer.at_line_start = True
        self.lexer.input(s)
        self.token_stream = self.top_filter(add_endmarker)

    def token(self):
        '''Returns next lexical token parsed from input.'''
        try:
            return next(self.token_stream)
        except StopIteration:
            return None

    def top_filter(self, add_endmarker=True):
        token = None  #TODO Is this line necessary?
        for token in self.filter_whitespace(iter(self.lexer.token, None)):
            yield token
        #TODO add_endmarker

    NONINDENTED_START = 1
    NONINDENTED_MIDLINE = 2
    INDENTED_MIDLINE = 3
    NEWLINE_AFTER_INDENTED = 4
    GOT_WS = 5

    def filter_whitespace(self, tokens):
        '''Removes NEWLINE and WS tokens and inserts appropriate INDENT and
        OUTDENT tokens.'''
        state = self.NONINDENTED_START

        for token in tokens:
            print('STATE', state, token)
            if state == self.NONINDENTED_START:
                if token.type == 'NEWLINE':
                    pass
                elif token.type == 'WS':
                    state = self.GOT_WS
                else:
                    state = self.NONINDENTED_MIDLINE
                    yield token
            elif state == self.GOT_WS:
                if token.type == 'NEWLINE':
                    state = self.NONINDENTED_START
                elif token.type == 'WS':
                    pass
                else:
                    state = self.INDENTED_MIDLINE
                    print('**INDENT')
                    yield INDENT(token.lineno)
                    yield token
            elif state == self.NONINDENTED_MIDLINE:
                if token.type == 'NEWLINE':
                    state = self.NONINDENTED_START
                elif token.type == 'WS':
                    pass
                else:
                    yield token
            elif state == self.INDENTED_MIDLINE:
                if token.type == 'NEWLINE':
                    state = self.NEWLINE_AFTER_INDENTED
                elif token.type == 'WS':
                    pass
                else:
                    yield token
            else:  # state == self.NEWLINE_AFTER_INDENTED
                if token.type == 'NEWLINE':
                    pass
                elif token.type == 'WS':
                    state = self.INDENTED_MIDLINE
                else:
                    state = self.NONINDENTED_MIDLINE
                    yield DEDENT(token.lineno)
                    yield token

        # Add DEDENT at end of file if indented
        if (
            state == self.INDENTED_MIDLINE
            or
            state == self.NEWLINE_AFTER_INDENTED
        ):
            yield DEDENT(token.lineno)


def _new_token(type, lineno):
    tok = lex.LexToken()
    tok.type = type
    tok.value = None
    tok.lineno = lineno
    return tok

def DEDENT(lineno):
    '''Synthesizes a DEDENT tag.'''
    return _new_token("DEDENT", lineno)

def INDENT(lineno):
    '''Synthesizes an INDENT tag.'''
    return _new_token("INDENT", lineno)


class Parser:
    '''Parser class that hooks up Indent1Lexer and PLY's yacc.'''

    def __init__(self, lexer=None):
        if lexer is None:
            #lexer = lex.lex(debug=1)
            lexer = Indent1Lexer(debug=1)
        self.lexer = lexer
        self.lexer.paren_count = 0
        self.lexer.at_line_start = True
        self.parser = yacc.yacc(debug=5)

    def parse(self, code):
        self.lexer.input(code)
        result = self.parser.parse(lexer=self.lexer)
        return result

class ClassDef:
    '''A 'class' parsed from the tiny language. A class has a .name and
    zero or more arguments, stored in .elems.'''
    def __init__(self, name, elems=None):
        self.name = name
        if elems is None:
            elems = []
        self.elems = elems

    __repr__ = nice_object_repr

########################################################################
#
# The grammar for the tiny language
#

def p_start(p):
    """start : prog """
    p[0] = p[1]

def p_prog(p):
    """prog : empty
            | prog classdef"""
    print('PROG', len(p))
    if len(p) == 2:  # if empty
        p[0] = []
    else:
        p[0] = p[1]
        p[0].append(p[2])

def p_classdef(p):
    """classdef : CLASS NAME
                | CLASS NAME INDENT class_elems DEDENT"""
    print('CLASSDEF', len(p))
    for i in range(len(p)):
        print(i, p[i])
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

parser = Parser()

prog1 = """
class Blah
class Gah INDENT a b DEDENT
class Whah
  c
class Vlah
  v
  w
class Hah
class Glah
    (a    # newline without indent
     b)
    c
    ((d e) ())
class Bah
    b
 x
"""

r1 = parser.parse(prog1)
print(r1)

prog2 = """

class Glah
"""

r2 = parser.parse(prog2)
print(r2)
