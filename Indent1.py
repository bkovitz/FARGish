# Indent1.py -- Wrapper classes for PLY's lex and yacc, to replace whitespace
#               and newlines with INDENT and DEDENT tokens.
#
# A caller should create a Parser object, passing it a PLY lexer and PLY
# semantic parser, and then call .parse() on that object. The PLY lexer is
# assumed to output WS and NEWLINE tokens and keep track of parentheses.

from ply import lex


class Indent1Lexer:

    def __init__(self, lexer):
        '''lexer is the PLY lexer with a grammar that generates WS and NEWLINE
        tokens. The Indent1Lexer postprocesses the token stream from the PLY
        lexer, replacing those tokens with INDENT and DEDENT.'''
        self.lexer = lexer
        self.token_stream = None

    def input(self, s):
        self.lexer.paren_count = 0  # must be updated by t_LPAREN and t_RPAREN
                                    # to prevent INDENT/DEDENT within
                                    # parentheses
        self.lexer.brace_count = 0  # similarly
        self.lexer.bracket_count = 0  # similarly
        self.lexer.at_line_start = True  # required by t_WS
        self.lexer.input(s)
        self.token_stream = self.top_filter()

    def token(self):
        '''Returns next lexical token parsed from input.'''
        try:
            return next(self.token_stream)
        except StopIteration:
            return None

    def top_filter(self):
        for token in self.filter_whitespace(iter(self.lexer.token, None)):
            yield token

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
            #print('STATE', state, token)
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
                    yield INDENT(token.lineno, token.lexpos)
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
                    yield DEDENT(token.lineno, token.lexpos)
                    yield token

        # Add DEDENT at end of file if indented
        if (
            state == self.INDENTED_MIDLINE
            or
            state == self.NEWLINE_AFTER_INDENTED
        ):
            yield DEDENT(token.lineno, token.lexpos)


def _new_token(type, lineno, lexpos):
    tok = lex.LexToken()
    tok.type = type
    tok.value = None
    tok.lineno = lineno
    tok.lexpos = lexpos
    return tok

def DEDENT(lineno, lexpos):
    '''Synthesizes a DEDENT tag.'''
    return _new_token("DEDENT", lineno, lexpos)

def INDENT(lineno, lexpos):
    '''Synthesizes an INDENT tag.'''
    return _new_token("INDENT", lineno, lexpos)


class Parser:
    '''Parser class that hooks up Indent1Lexer and PLY's yacc.'''

    def __init__(self, ply_lexer, ply_yacc):
        '''Pass the PLY lexer and PLY syntactic parser defined in the calling
        module, like this: Parser(lex.lex(), yacc.yacc().'''
        self.lexer = Indent1Lexer(ply_lexer)
        self.parser = ply_yacc

    def parse(self, code, debug=False):
        self.lexer.input(code)
        result = self.parser.parse(lexer=self.lexer, debug=debug)
        return result
