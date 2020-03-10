'''
Simplest
  => do_something()
'''

def actions(self, _g, _thisid):
    _result = []
    _result.append(do_something())
    return _result

'''
Builder
  => build ConsumeOperands(1, 2, 3)
'''
def actions(self, _g, _thisid):
    _result = []
    _result.append(do_something())
    return _result

'''
Scout4
  see t1 := NodeOfClass(Target)
  => add_tag(Found, t1)
  else t2 := NodeOfClass(Found)
  => add_tag(Done, t2)
  else t3 := NodeOfClass(SomethingElse)
  => do_something(Found, t2, t3)
'''
def actions(self, _g, _thisid):
    _result = []
    t1 = NodeOfClass(Target).see_one()
    if t1 is not None:
        _result.append(add_tag(Found, t1))
    else:
        t2 = NodeOfClass(Found)
        if t2 is not None:
            result.append(add_tag(Done, t2))
        else:
            t3 = NodeOfClass(SomethingElse)
            if t3 is not None:
                result.append(do_something(Found, t2, t3))
    return _result

'''
OperandsScout(target)
  see p1 := NodeWithTag(Number, Avail)
      p2 := NodeWithTag(Number, Avail)
      op := NodeWithTag(Operator, Allowed)
  => build ConsumeOperands(consume_operands=[p1, p2], proposed_operator=op)
'''
def actions(self, _g, _thisid):
    _result = []
    p1 = p2 = op = None
    _found_tup = CartesianProduct(
        NodeWithTag(Number, Avail),
        NodeWithTag(Number, Avail),
        NodeWithTag(Operator, Allowed),
        whole_tuple_criterion=TupAnd(
            no_dups,
            NotAlreadyBuilt(ConsumeOperands)
        )
    ).see_one(_g)
    if _found_tup:
        p1, p2, op = _found_tup
    if _found_tup:
        _result.append(Build2.maybe_make(ConsumeOperands, _g, kwargs={'consume_operands': [p1, p2], 'proposed_operator': op}))
    return _result

'''
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
def actions(self, _g, _thisid):
    _result = []
    p1 = p2 = op = None
    _found_tup = CartesianProduct(
        NodeWithTag(Number, Avail),
        NodeWithTag(Number, Avail),
        NodeWithTag(Operator, Allowed),
        whole_tuple_criterion=TupAnd(
            no_dups,
            NotAlreadyBuilt(ConsumeOperands)
        )
    ).see_one(_g)
    if _found_tup:
        p1, p2, op = _found_tup
    if _found_tup:
        _result.append(Build2.maybe_make(ConsumeOperands, _g, kwargs={'consume_operands': [p1, p2], 'proposed_operator': op}))
    else:
        block = NodeWithTag(Block, Avail).see_one(_g)
        if block is not None and block != _g.neighbor(_thisid, 'target'):
            _result.append(Fail(block))
    return _result

