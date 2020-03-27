# testBrute.py -- FARGish test: solve a simple numble by brute-force search
#
# This is more than a unit test: it exercises parsing of FARGish, generating
# Python code, and running the model. It therefore tests nearly the whole
# system. The code here also demonstrates how to set up and run a FARG model
# with FARGish.

import unittest

from Numble import make_numble_class
from PortGraph import PortGraph, pg, ps
from codegen import make_python, compile_fargish
from log import *
from exc import FargDone
import expr
import support
from ExprAsEquation import ExprAsEquation

prog = '''
tags -- taggees
consume_operand -- proposer
proposed_operator -- proposer
behalf_of -- agents
target -- tags

Tag(taggees)
Avail, Consumed, Failed, Done, Allowed, Promising, Hopeless : Tag
GettingCloser : Promising

Workspace

Number(value)
Brick, Target, Block : Number

Operator
Plus, Times : Operator
Minus(minuend, subtrahend) : Operator

Want : Tag
  agent: OperandsScout(target=taggees)
  agent: SuccessScout(target=taggees)

OperandsScout(target)
  see p1 := NodeWithTag(Number, Avail),
      p2 := NodeWithTag(Number, Avail),
      op := NodeWithTag(Operator, Allowed)
  => build ConsumeOperands(op, p1, p2)
  else see block := NodeWithTag(Block, Avail), block != target
  => Fail(block)

SuccessScout(target)
  see winner := NodeWithValue(target.value, nodeclass=Number, tagclass=Avail)
  => succeeded(winner, target)

ConsumeOperands(consume_operand, consume_operand, proposed_operator)
  see Not(Tagged(Done, this)), AllTagged(Avail, consume_operand(this))
  => consumeOperands(this)  # external func; complicated
'''

make_python(prog, debug=1)
exec(compile_fargish(prog), globals())
