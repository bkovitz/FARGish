# testGen.py -- Tests of code-generation classes in gen.py
#
# Unit tests in this file compare against literal Python code. These illustrate
# what Python code is generated from FARGish. There are many correct ways to
# generate code, though, so if a refactoring of code generation causes these
# tests to fail, it is more likely that the tests need to be updated than that
# there is an error in the code generation.

import unittest
from io import StringIO
from pprint import pprint as pp

from gen import gen, IfStmt, NullStmt, BuildStmt, NodeclassExpr, FuncCall, \
    VarRef, TupleExpr, ArgExpr, CartProdExpr, NodeSearch, Relexpr, \
    ActionExpr, PortLabel
from Env import Env
from Indenting import Indenting


def test_gen(o, env):
    file = Indenting(StringIO())
    fixup = Indenting(StringIO())
    print(file=file)  # This makes the 'expect' string in the unit tests
                      # easier to write and read.
    gen(o, file, fixup, env)
    fixup.seek(0)
    for line in fixup:
        print(line, file=file, end='')
    return file.getvalue()

class TestGen(unittest.TestCase):

    def test_ifstmt_null(self):
        env = Env()
        ifstmt = IfStmt(None, NullStmt(), None)
        #print()
        #print(test_gen(ifstmt, env))
        self.assertEqual(test_gen(ifstmt, env), '''
pass
''')

    def test_ifstmt_build(self):
        env = Env()
        then = BuildStmt(NodeclassExpr('Node'), [])
        ifstmt = IfStmt(None, then, None)
        self.assertEqual(test_gen(ifstmt, env), '''
if not _g.is_already_built(Node, args=[], kwargs={}):
    _result.append(make_build(_g, Node, args=[], kwargs={}))
''')

    def test_ifstmt_nodesearch(self):
        env = Env()
        fc = FuncCall(
               funcname='NodeOfClass',
               args=[
                 ArgExpr(
                   argname=None,
                   expr=TupleExpr(VarRef(name='Brick'),
                                  VarRef(name='Block')))])
        cond = CartProdExpr([NodeSearch(name='node', expr=fc)], [])
        ifstmt = IfStmt(cond, NullStmt(), None)
        self.assertEqual(test_gen(ifstmt, env), '''
node = None
_found_tup_1 = CartesianProduct(NodeOfClass((Brick, Block))).see_one(_g)
if _found_tup_1:
    node, = _found_tup_1
if _found_tup_1:
    pass
''')

    def test_ifstmt_nodesearch2(self):
        #   see p1 := NodeWithTag(Number, Avail),
        #       p2 := NodeWithTag(Number, Avail),
        #       op := NodeWithTag(Operator, Allowed)
        #   => build ConsumeOperands(op, p1, p2)
        env = Env()
        p1fc = FuncCall(funcname='NodeWithTag', args=[ArgExpr(argname=None, expr=VarRef(name='Number')), ArgExpr(argname=None, expr=VarRef(name='Avail'))])
        p2fc = FuncCall(funcname='NodeWithTag', args=[ArgExpr(argname=None, expr=VarRef(name='Number')), ArgExpr(argname=None, expr=VarRef(name='Avail'))])
        opfc = FuncCall(funcname='NodeWithTag', args=[ArgExpr(argname=None, expr=VarRef(name='Operator')), ArgExpr(argname=None, expr=VarRef(name='Allowed'))])
        then = BuildStmt(nodeclass_expr=NodeclassExpr('ConsumeOperands'), args=[ArgExpr(argname=None, expr=VarRef(name='op')), ArgExpr(argname=None, expr=VarRef(name='p1')), ArgExpr(argname=None, expr=VarRef(name='p2'))])
        cond = CartProdExpr([
            NodeSearch(name='p1', expr=p1fc),
            NodeSearch(name='p2', expr=p2fc),
            NodeSearch(name='op', expr=opfc)
        ], whole_tuple_criteria=[])
        ifstmt = IfStmt(cond, then, None)
        self.assertEqual(test_gen(ifstmt, env), '''
p1 = p2 = op = None
def _f_2(_g, _tup):
    p1, p2, op, = _tup
    return not _g.is_already_built(ConsumeOperands, args=[op, p1, p2], kwargs={})
_found_tup_1 = CartesianProduct(NodeWithTag(Number, Avail), NodeWithTag(Number, Avail), NodeWithTag(Operator, Allowed), whole_tuple_criterion=TupAnd(no_dups, TupFunc(_f_2))).see_one(_g)
if _found_tup_1:
    p1, p2, op, = _found_tup_1
if _found_tup_1:
    _result.append(make_build(_g, ConsumeOperands, args=[op, p1, p2], kwargs={}))
''')

    def test_ifstmt_everything(self):
        #   see p1 := NodeWithTag(Number, Avail),
        #       p2 := NodeWithTag(Number, Avail),
        #       op := NodeWithTag(Operator, Allowed)
        #   => build ConsumeOperands(op, p1, p2)
        #   else see block := NodeWithTag(Block, Avail), block != target
        #   => Fail(block)
        env = Env()

        target = PortLabel('target')
        tags = PortLabel('tags')
        target.add_link_to(tags)
        target.add_to_env(env)
        tags.add_to_env(env)

        # First 'see'
        p1fc = FuncCall(funcname='NodeWithTag', args=[ArgExpr(argname=None, expr=VarRef(name='Number')), ArgExpr(argname=None, expr=VarRef(name='Avail'))])
        p2fc = FuncCall(funcname='NodeWithTag', args=[ArgExpr(argname=None, expr=VarRef(name='Number')), ArgExpr(argname=None, expr=VarRef(name='Avail'))])
        opfc = FuncCall(funcname='NodeWithTag', args=[ArgExpr(argname=None, expr=VarRef(name='Operator')), ArgExpr(argname=None, expr=VarRef(name='Allowed'))])
        then = BuildStmt(nodeclass_expr=NodeclassExpr('ConsumeOperands'), args=[ArgExpr(argname=None, expr=VarRef(name='op')), ArgExpr(argname=None, expr=VarRef(name='p1')), ArgExpr(argname=None, expr=VarRef(name='p2'))])
        cond1 = CartProdExpr([
            NodeSearch(name='p1', expr=p1fc),
            NodeSearch(name='p2', expr=p2fc),
            NodeSearch(name='op', expr=opfc)
        ], whole_tuple_criteria=[])
        then1 = BuildStmt(nodeclass_expr=NodeclassExpr('ConsumeOperands'), args=[ArgExpr(argname=None, expr=VarRef(name='op')), ArgExpr(argname=None, expr=VarRef(name='p1')), ArgExpr(argname=None, expr=VarRef(name='p2'))])

        # Second 'see' (after 'else')
        blockfc = FuncCall(funcname='NodeWithTag', args=[ArgExpr(argname=None, expr=VarRef('Block')), ArgExpr(argname=None, expr=VarRef('Avail'))])
        wtc2 = Relexpr(lhs=VarRef('block'), op='!=', rhs=VarRef('target'))
        cond2 = CartProdExpr([
            NodeSearch(name='block', expr=blockfc)
        ], whole_tuple_criteria=[wtc2])
        then2 = ActionExpr(FuncCall(funcname='Fail', args=[ArgExpr(argname=None, expr=VarRef('block'))]))

        ifstmt = IfStmt(cond1, then1, IfStmt(cond2, then2, None))
        ifstmt.add_to_env(env)

        self.assertEqual(test_gen(ifstmt, env), '''
p1 = p2 = op = None
def _f_2(_g, _tup):
    p1, p2, op, = _tup
    return not _g.is_already_built(ConsumeOperands, args=[op, p1, p2], kwargs={})
_found_tup_1 = CartesianProduct(NodeWithTag(Number, Avail), NodeWithTag(Number, Avail), NodeWithTag(Operator, Allowed), whole_tuple_criterion=TupAnd(no_dups, TupFunc(_f_2))).see_one(_g)
if _found_tup_1:
    p1, p2, op, = _found_tup_1
if _found_tup_1:
    _result.append(make_build(_g, ConsumeOperands, args=[op, p1, p2], kwargs={}))
else:
    block = None
    def _f_4(_g, _tup):
        block, = _tup
        return block != _g.neighbor(_thisid, 'target')
    _found_tup_3 = CartesianProduct(NodeWithTag(Block, Avail), whole_tuple_criterion=TupFunc(_f_4)).see_one(_g)
    if _found_tup_3:
        block, = _found_tup_3
    if _found_tup_3:
        _result.append(Fail(block))
''')
