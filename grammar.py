# grammar.py -- The grammar for FARGish
#
# Written in PLY (Python Lex Yacc): https://www.dabeaz.com/ply/
#
# A caller should import the 'parser' object.

import ply.lex as lex
import ply.yacc as yacc

from pprint import pprint as pp

from Indent1 import Parser
from gen import ExtFunc, ExtGFunc, LinkDefn, NodeHeader, NameWithArguments, \
    NodeDefn, VarRef, Constant, FuncCall, MemberChain, \
    Relexpr, LetExpr, SeeDo2, AgentExpr, ArgExpr, Initializer, \
    ConditionsWithActions, ConditionWithActions, NodeclassExpr, \
    BuildStmt, ActionExpr, ConditionExpr2, TupleExpr, NodeSearch, \
    ThisExpr, CartProdExpr, coalesce_conditions
from util import as_iter, as_list


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
    'INTEGER',
    'FLOAT',
    'STRING',
    'DOT',
    'FUNCS',
    'GFUNCS',
    'DOUBLE_HYPHEN',
    'AGENT',
    'FAT_RIGHT_ARROW',
    'SEE',
    'ELSE',
    'LET',
    'BUILD',
    'THIS',
    'NODESEARCH',
    'EQ',
    'NE',
    'LT',
    'LE',
    'GT',
    'GE',
    'ENDMARKER'
)

literals = ',:='

def t_FLOAT(t):
    r'[-+]?[0-9]+(\.([0-9]+)?([eE][-+]?[0-9]+)?|[eE][-+]?[0-9]+)'
    t.value = float(t.value)
    # TODO error message if the number is invalid
    return t

def t_INTEGER(t):
    r'[-+]?[0-9]+'
    t.value = int(t.value)
    # TODO error message if the number is invalid
    return t

def t_STRING(t):
    r'(?:"(?:[^"\\n\\r\\\\]|(?:"")|(?:\\\\x[0-9a-fA-F]+)|(?:\\\\.))*")|(?:\'(?:[^\'\\n\\r\\\\]|(?:\'\')|(?:\\\\x[0-9a-fA-F]+)|(?:\\\\.))*\')'
    return t

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

def t_DOUBLE_HYPHEN(t):
    r'--'
    return t

t_DOT = r'\.'
    
KEYWORDS = {
    'funcs': 'FUNCS',
    'gfuncs': 'GFUNCS',
    'agent': 'AGENT',
    'see': 'SEE',
    'else': 'ELSE',
    'build': 'BUILD',
    'this': 'THIS',
    'NodeSpec': 'NODESEARCH',
    'NodeOfClass': 'NODESEARCH',
    'NodeWithTag': 'NODESEARCH',
    'NodeWithValue': 'NODESEARCH',
    'NodeWithNeighborAt': 'NODESEARCH'
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


##### Helpers for syntactic analyzer

def zero_or_more(p, elem_index):
    '''Helps build up a list of zero or more items parsed in the PLY grammar.
    If p does not contain an item at elem_index (i.e. p is too short), puts an
    empty list in p[0]. Otherwise, appends p[elem_index] to the list assumed to
    be in p[1], and exits with that list in p[0]. If p[elem_index] is itself a
    list, all its elements are added to the end of the list.'''
    if len(p) <= elem_index:
        p[0] = []
    else:
        p[0] = p[1]
        if isinstance(p[elem_index], list):
            p[0] += p[elem_index]
        else:
            p[0].append(p[elem_index])

def one_or_more(p, initial_item_index, additional_item_index):
    '''Helps build up a list of one or more items parsed in the PLY grammar.
    For the first item, p[initial_item_index] should contain the item and p
    should contain no elements beyond that one. For all other items,
    p[initial_item_index] should contain the list built up so far and
    p[additional_item_index] should contain the next item. On exit, the
    list so far is in p[0].'''
    if len(p) == initial_item_index + 1:
        p[0] = [p[initial_item_index]]
    else:
        p[0] = p[1]
        p[0].append(p[additional_item_index])


##### Grammar for syntactic analyzer

precedence = (
    ('left', ','),
    ('left', 'LPAREN', 'RPAREN'),
)

def p_prog(p):
    '''prog : empty
            | prog prog_elem'''
    zero_or_more(p, 2)

def p_prog_elem(p):
    '''prog_elem : funcs
                 | gfuncs
                 | link_defn
                 | node_defn'''
    p[0] = p[1]

def p_funcs(p):
    '''funcs : FUNCS LBRACE maybe_names RBRACE'''
    p[0] = [ExtFunc(name) for name in p[3]]

def p_gfuncs(p):
    '''gfuncs : GFUNCS LBRACE maybe_names RBRACE'''
    p[0] = [ExtGFunc(name) for name in p[3]]

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
    #'''agent_defn : AGENT ':' expr'''
    '''agent_defn : AGENT ':' buildspec'''
    #p[0] = AgentExpr(p[3])
    #print('AGDEF', p.lineno(3), p.lexpos(3)) #, p.linespan(3), p.lexspan(3))
    #p[0] = SeeDo([ConditionsWithActions([], [p[3].as_agent_expr()])])
    p[0] = SeeDo2(None, p[3].as_agent_stmt(), None)

#def p_implicit_see_do(p):
#    '''see_do : unconditional_actions maybe_else_see_do'''
#    p[0] = SeeDo([p[1]] + p[2])
#
#def p_explicit_see_do(p):
#    '''see_do : SEE conditional_actions maybe_else_see_do'''
#    p[0] = SeeDo([p[2]] + p[3])

def p_see_do(p):
    '''see_do : SEE conditions FAT_RIGHT_ARROW actions maybe_else_see_do
              | FAT_RIGHT_ARROW actions maybe_else_see_do'''
    if len(p) == 6:
        p[0] = SeeDo2(make_cartprodexpr(p[2]), p[4], p[5])
    else:
        p[0] = SeeDo2(None, p[2], p[3])

def make_cartprodexpr(conditions):
    nodesearches = []
    whole_tuple_exprs = []
    for condition in conditions:
        if isinstance(condition, NodeSearch):
            nodesearches.append(condition)
        else:
            whole_tuple_exprs.append(condition)
    if nodesearches:
        return CartProdExpr(nodesearches, whole_tuple_exprs)
    else:
        return coalesce_conditions(whole_tuple_exprs, [])

#def p_unconditional_actions(p):
#    '''unconditional_actions : FAT_RIGHT_ARROW actions'''
#    p[0] = ConditionsWithActions([], p[2])
#    #p[0] = ConditionWithActions(ConditionExpr(), p[2])
#
#def p_conditional_actions(p):
#    '''conditional_actions : conditions FAT_RIGHT_ARROW actions'''
#    p[0] = ConditionsWithActions(p[1], p[3])
#    #p[0] = ConditionWithActions(p[1], p[3])

def p_maybe_else_see_do(p):
    '''maybe_else_see_do : empty
                         | ELSE see_do'''
    if len(p) == 2:
        p[0] = None
    else:
        p[0] = p[2]

#def p_maybe_else_chain(p):
#    '''maybe_else_chain : empty
#                        | maybe_else_chain ELSE unconditional_actions
#                        | maybe_else_chain ELSE conditional_actions'''
#    zero_or_more(p, 3)

#def p_see_do1(p):
#    '''see_do : FAT_RIGHT_ARROW actions'''
#    p[0] = SeeDo()
#
#def p_see_do2(p):
#    '''see_do : SEE conditions FAT_RIGHT_ARROW actions'''
#    p[0] = SeeDo(p[2], p[4], [], [])
#    p[0] = SeeDo([ConditionWithActions(p[2], p[4])
#
#def p_see_do3(p):
#    '''see_do : SEE conditions FAT_RIGHT_ARROW actions ELSE FAT_RIGHT_ARROW actions'''
#    p[0] = SeeDo(p[2], p[4], [], p[7])
#
#def p_see_do4(p):
#    '''see_do : SEE conditions FAT_RIGHT_ARROW actions ELSE conditions FAT_RIGHT_ARROW actions'''
#    p[0] = SeeDo(p[2], p[4], p[6], p[8])

def p_conditions(p):
    '''conditions : condition
                  | conditions ',' condition'''
    one_or_more(p, 1, 3)
#    # Returns a single ConditionExpr (which may hold multiple conditions)
#    if len(p) == 2:
#        p[0] = p[1]
#    else:
#        p[0] = p[1].add_condition_expr(p[3])

def p_condition(p):
    '''condition : expr
                 | nodesearch'''
                 #| NAME LET expr'''
    if len(p) == 2:
        p[0] = p[1] #ConditionExpr2(p[1])
    else:
        p[0] = ConditionExpr2(LetExpr(p[1], p[3]))  #TODO Just LetExpr?

# def p_nodesearches(p):
#     '''nodesearches : nodesearch
#                     | expr
#                     | nodesearches ',' nodesearch
#                     | nodesearches ',' expr'''
#     one_or_more(p, 1, 3)

def p_nodesearch(p):
    '''nodesearch : NODESEARCH
                  | NODESEARCH LPAREN maybe_args RPAREN
                  | NAME LET NODESEARCH
                  | NAME LET NODESEARCH LPAREN maybe_args RPAREN'''
    if len(p) == 2:
        p[0] = NodeSearch('_', FuncCall(p[1], []))
    elif len(p) == 5:
        p[0] = NodeSearch('_', FuncCall(p[1], p[3]))
    elif len(p) == 4:
        p[0] = NodeSearch(p[1], FuncCall(p[3], []))
    elif len(p) == 7:
        p[0] = NodeSearch(p[1], FuncCall(p[3], p[5]))
    else:
        assert(False, "Shouldn't get here.")

def p_actions(p):
    '''actions : action
               | actions ',' action'''
    one_or_more(p, 1, 3)

# NEXT   Build nodespec_expr
def p_action(p):
    '''action : BUILD buildspec
              | expr'''
    if len(p) == 3:
        p[0] = p[2]
    else:
        p[0] = ActionExpr(p[1])

def p_buildspec(p):
    '''buildspec : nodeclass LPAREN maybe_args RPAREN
                 | nodeclass'''
    if len(p) == 2:
        p[0] = BuildStmt(p[1], [])
    else:
        p[0] = BuildStmt(p[1], p[3])

def p_nodeclass(p):
    '''nodeclass : NAME'''
    p[0] = NodeclassExpr(p[1])

def p_expr(p):
    '''expr : tuple
            | constant
            | funccall
            | relexpr
            | member_chain
            | varref
            | this'''
    p[0] = p[1]

def p_exprs(p):
    '''exprs : expr
             | exprs ',' expr'''
    one_or_more(p, 1, 3)

def p_varref(p):
    '''varref : NAME'''
    p[0] = VarRef(p[1])

def p_this(p):
    '''this : THIS'''
    p[0] = ThisExpr()

def p_constant(p):
    '''constant : INTEGER
                | FLOAT
                | STRING'''
    p[0] = Constant(p[1])

def p_funccall(p):
    '''funccall : NAME LPAREN maybe_args RPAREN'''
    p[0] = FuncCall(p[1], p[3])

def p_relexpr(p):
    '''relexpr : expr relop expr'''
    p[0] = Relexpr(p[1], p[2], p[3])

def p_member_chain(p):
    '''member_chain : objref member_refs'''
    p[0] = MemberChain([p[1]] + p[2])

def p_objref(p):
    '''objref : varref
              | funccall'''
    p[0] = p[1]

def p_member_refs(p):
    '''member_refs : DOT objref
                   | member_refs DOT objref'''
    one_or_more(p, 2, 3)

def p_relop(p):
    '''relop : EQ
             | NE
             | LT
             | LE
             | GT
             | GE'''
    p[0] = p[1]

def p_tuple(p):
    '''tuple : tuple0
             | tuple_many
             | tuple1'''
    p[0] = p[1]

def p_tuple0(p):
    '''tuple0 : LPAREN RPAREN'''
    p[0] = TupleExpr()

def p_tuple1(p):
    '''tuple1 : LPAREN exprs ',' RPAREN'''
    p[0] = TupleExpr(*p[2])

def p_tuple_many(p):
    '''tuple_many : LPAREN exprs RPAREN'''
    p[0] = TupleExpr(*p[2])

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

def parse(code, predefs=None, debug=False):
    result = as_list(predefs)
    for i in parser.parse(code, tracking=True, debug=debug):
        for item in as_iter(i):
            result.append(item)
    return result

#TODO rm this test code; make a UT
if __name__ == '__main__':
    from Env import Env
    from gen import gen
    from Indenting import Indenting, indent
    from io import StringIO
    prog1 = '''funcs { arithResult, succeeded }
tags -- taggees
target -- tags

Tag
Avail, Allowed : Tag

Number(n)
Brick, Block : Number
Operator

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
    prog3 = '''
SuccessScout(target)
  see winner := NodeWithValue(target.value, nodeclass=Number, tagclass=Avail)
  => succeeded(winner, target)
'''
    prog4 = '''
Blah
    agent: Blah(2, 3.4, "foo", 'goo')
'''
    got = parser.parse(prog1)
    #pp(got)
    env = Env(got)
    defn = env['OperandsScout']
    cas = defn.body[0].cas
    #ca = defn.body[0].cas[0]  # the first conditions-actions pair
    #c = ca.conditions[0]  # The first NodeWithTag
    g = cas[0].make_gen(env, cas[1:])
    s1 = StringIO()
    s2 = StringIO()
    gen(g, Indenting(s1), Indenting(s2), env)
    print(s1.getvalue(), end='')
    print(s2.getvalue(), end='')
    #pp(got[0].body[0])
