# demo3.py -- Numbo made by analogy with Jumbo

'''
Elements of expressions join into Gloms.
'''

from codegen import make_python, compile_fargish

prog = '''
tags -- taggees
consume-operand -- proposer
proposed-operator -- proposer
behalf_of -- agents
target -- tags

Workspace

Tag(taggees)
CouldBeOperand : Tag

Number(value)
Brick, Target, Block : Number

Operator
Plus, Times : Operator
Minus(minuend, subtrahend) : Operator

OperandTagger
  see n := NodeOfClass((Brick, Block))
  => build CouldBeOperand(n)
'''

make_python(prog)
exec(compile_fargish(prog), globals())
