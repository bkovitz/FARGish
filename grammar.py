# grammar.py -- The grammar for FARGish
#
# Written in PLY (Python Lex Yacc): https://www.dabeaz.com/ply/
#
# A caller should import the 'parser' object.

import ply.lex as lex
import ply.yacc as yacc

from pprint import pprint as pp

from Indent1 import Parser
from gen import ExternalList, LinkDefn, NodeHeader, NameWithArguments, \
    NodeDefn, BuildExpr, VarRef, FuncCall, Relop, LetExpr, SeeDo, AgentExpr, \
    ArgExpr, Initializer


##### Grammar for lexical analyzer

tokens = (
    'NAME',
    'LPAREN',
    'RPAREN',
    'LBRACE',
    'RBRACE',
    'INDENT',
    'DEDENT',
    'WS', # white space
    'NEWLINE',
    'EXTERNAL',
    'DOUBLE_HYPHEN',
    'AGENT',
    'FAT_RIGHT_ARROW',
    'SEE',
    'ELSE',
    'LET',
    'BUILD',
    'EQ',
    'NE',
    'LT',
    'LE',
    'GT',
    'GE',
    'ENDMARKER'
)

literals = ',:='

def t_FAT_RIGHT_ARROW(t):
    r'=>'
    t.type = 'FAT_RIGHT_ARROW'
    return t

def t_EQ(t):
    r'=='
    t.type = 'EQ'
    return t

def t_NE(t):
    r'!='
    t.type = 'NE'
    return t

def t_LE(t):
    r'<='
    t.type = 'LE'
    return t

def t_LT(t):
    r'<'
    t.type = 'LT'
    return t

def t_GE(t):
    r'>='
    t.type = 'GE'
    return t

def t_GT(t):
    r'>'
    t.type = 'GT'
    return t

def t_LET(t):
    r':='
    t.type = 'LET'
    return t

def t_COLON(t):
    r':'
    t.type = ':'
    return t

def t_LPAREN(t):
    r'\('
    t.lexer.paren_count += 1
    return t

def t_RPAREN(t):
    r'\)'
    # check for underflow?  should be the job of the parser
    t.lexer.paren_count -= 1
    return t

def t_LBRACE(t):
    r'\{'
    t.lexer.brace_count += 1
    return t

def t_RBRACE(t):
    r'\}'
    t.lexer.brace_count -= 1
    return t

#TODO rm?
def t_DOUBLE_HYPHEN(t):
    r'--'
    return t
    
KEYWORDS = {
    'external': 'EXTERNAL',
    'agent': 'AGENT',
    'see': 'SEE',
    'else': 'ELSE',
    'build': 'BUILD'
}

def t_NAME(t):
    r'[-a-zA-Z_\<>!\?][-a-zA-Z0-9_<>!\?]*'
    t.type = KEYWORDS.get(t.value, 'NAME')
    return t

# t_comment appears ahead of t_WS so that t_comment consumes entire lines
# containing only comments, so t_WS never sees them. t_comment does not
# consume the newline.
def t_comment(t):
    r"[ ]*\043[^\n]*"  # \043 is '#'
    pass

def counts_are_zero(t):
    return (
        t.lexer.paren_count == 0
        and
        t.lexer.brace_count == 0
    )

# Whitespace
def t_WS(t):
    r'[ ]+'
    if t.lexer.at_line_start and counts_are_zero(t):
        return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    t.type = 'NEWLINE'
    # Don't generate newline tokens when inside of parenthesis, eg
    #   a = (1,
    #        2, 3)
    if counts_are_zero(t):
        return t

def t_error(t):
    raise SyntaxError("Unknown symbol %r" % (t.value[0],))
    print("Skipping", repr(t.value[0]))
    t.lexer.skip(1)


##### Grammar for syntactic analyzer

def zero_or_more(p, elem_index):
    if len(p) <= elem_index:
        p[0] = []
    else:
        p[0] = p[1]
        if isinstance(p[elem_index], list):
            p[0] += p[elem_index]
        else:
            p[0].append(p[elem_index])

def one_or_more(p, initial_item_index, additional_item_index):
    if len(p) == initial_item_index + 1:
        p[0] = [p[initial_item_index]]
    else:
        p[0] = p[1]
        p[0].append(p[additional_item_index])

def p_prog(p):
    '''prog : empty
            | prog prog_elem'''
    zero_or_more(p, 2)

def p_prog_elem(p):
    '''prog_elem : external_list
                 | link_defn
                 | node_defn'''
    p[0] = p[1]

def p_external_list(p):
    '''external_list : EXTERNAL LBRACE maybe_names RBRACE'''
    p[0] = ExternalList(p[3])

def p_link_defn(p):
    '''link_defn : NAME DOUBLE_HYPHEN NAME'''
    p[0] = LinkDefn(p[1], p[3])

def p_node_defn(p):
    '''node_defn : node_header
                 | node_header INDENT node_body DEDENT'''
    body = []
    if len(p) == 5:
        body = p[3]
    p[0] = p[1].make_node_defns(body)

def p_node_header(p):
    '''node_header : node_names
                   | node_names ':' node_names'''
    if len(p) == 2:
        p[0] = NodeHeader(p[1], [])
    else:
        p[0] = NodeHeader(p[1], p[3])

def p_node_body(p):
    '''node_body : empty
                 | node_body node_body_elem'''
    zero_or_more(p, 2)

def p_node_body_elem(p):
    '''node_body_elem : initializer
                      | agent_defn
                      | see_do'''
    p[0] = p[1]

def p_initializer(p):
    '''initializer : NAME '=' expr'''
    p[0] = Initializer(p[1], p[3])

def p_agent_defn(p):
    '''agent_defn : AGENT ':' expr'''
    p[0] = AgentExpr(p[3])

def p_see_do1(p):
    '''see_do : FAT_RIGHT_ARROW actions'''
    p[0] = SeeDo([], p[2], [], [])

def p_see_do2(p):
    '''see_do : SEE conditions FAT_RIGHT_ARROW actions'''
    p[0] = SeeDo(p[2], p[4], [], [])

def p_see_do3(p):
    '''see_do : SEE conditions FAT_RIGHT_ARROW actions ELSE FAT_RIGHT_ARROW actions'''
    p[0] = SeeDo(p[2], p[4], [], p[7])

def p_see_do4(p):
    '''see_do : SEE conditions FAT_RIGHT_ARROW actions ELSE conditions FAT_RIGHT_ARROW actions'''
    p[0] = SeeDo(p[2], p[4], p[6], p[8])

def p_conditions(p):
    '''conditions : condition
                  | conditions ',' condition'''
    one_or_more(p, 1, 3)

def p_condition(p):
    '''condition : expr
                 | NAME LET expr'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = LetExpr(p[1], p[3])

def p_actions(p):
    '''actions : action
               | actions ',' action'''
    one_or_more(p, 1, 3)

def p_action(p):
    '''action : BUILD expr
              | expr'''
    if len(p) == 3:
        p[0] = BuildExpr(p[2])
    else:
        p[0] = p[1]

def p_varref(p):
    '''expr : NAME'''
    p[0] = VarRef(p[1])

def p_funccall(p):
    '''expr : NAME LPAREN maybe_args RPAREN'''
    p[0] = FuncCall(p[1], p[3])

def p_relexpr(p):
    '''expr : expr relop expr'''
    p[0] = Relop(p[1], p[2], p[3])

def p_relop(p):
    '''relop : EQ
             | NE
             | LT
             | LE
             | GT
             | GE'''
    p[0] = p[1]

def p_maybe_args(p):
    '''maybe_args : empty
                  | args'''
    p[0] = p[1]

def p_args(p):
    '''args : arg
            | args ',' arg'''
    one_or_more(p, 1, 3)

def p_arg(p):
    '''arg : expr
           | NAME '=' expr'''
    if len(p) == 2:
        p[0] = ArgExpr(None, p[1])
    else:
        p[0] = ArgExpr(p[1], p[3])

def p_node_names(p):
    '''node_names : node_name
                  | node_names ',' node_name'''
    one_or_more(p, 1, 3)

def p_node_name(p):
    '''node_name : NAME
                 | NAME LPAREN names RPAREN'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = NameWithArguments(p[1], p[3])

def p_names(p):
    '''names : NAME
             | names ',' NAME'''
    one_or_more(p, 1, 3)

def p_maybe_names(p):
    '''maybe_names : empty
                   | names'''
    p[0] = p[1]

def p_error(p):
    if p:
        print(f"Syntax error at '{p.value}' in line {p.lineno}: did not expect {p.type}")
        #TODO Show the column, and/or maybe the whole line. To do that, we'll
        #access to the string for the whole input program. PLY does not
        #seem to pass this to p_error().
        #print(p.lexpos, type(p.lexer), dir(p.lexer))
    else:
        print("Syntax error: unexpected end of input.")
    #print('ERROR', p.type, p.value, p.lineno, p.lexpos)

def find_line(s, index):
    lb = s.rfind('\n', 0, index) + 1
    ub = s.find('\n', index)
    return s[lb:ub]

def p_empty(p):
    '''empty :'''
    p[0] = []  # empty list


parser = Parser(lex.lex(), yacc.yacc())

def parse(code, debug=None):
    return parser.parse(code, debug=debug)

#TODO rm this test code; make a UT
if __name__ == '__main__':
    prog1 = '''external { arithResult, succeeded }
tags -- taggees

Brick, Block : Number(n)

OperandsScout(behalf_of, target)
  see p1 := NodeWithTag(Number, Avail),
      p2 := NodeWithTag(Number, Avail),
      op := NodeWithTag(Operator, Allowed)
  => build ConsumeOperands(op, p1, p2)
  else block := NodeWithTag(Block, Avail), block != target
  => Fail(block)
'''
    prog2 = '''
Number(n)
  value = n

Brick(x), Block : Number(n)

Target: A
    '''
    got = parser.parse(prog1)
    #got = parser.parse(prog2)
    pp(got)
