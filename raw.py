# raw.py -- Classes holding 'raw' parsed elements of FARGish, generated
#           directly by the syntactic analyzer in grammar.py
#
# These classes, in turn, can generate Python code.

from util import NiceRepr

class NodeDef(NiceRepr):

    def __init__(self, name, ancestor_names=None, initializers=None):
        if ancestor_names is None:
            ancestor_names = []
        if initializers is None:
            initializers = []
        self.name = name
        self.ancestor_names = ancestor_names
        self.initializers = initializers

    def gen(self, file, env):
        if self.ancestor_names:
            ancs = ', '.join(get_name(a) for a in self.ancestor_names)
        else:
            ancs = 'Node'
        print(f'''
class {get_name(self.name)}({ancs}):
    pass''', file=file)

def get_name(o):
    try:
        return o.name
    except AttributeError:
        return o

class NameWithArguments(NiceRepr):

    def __init__(self, name, arguments):
        self.name = name
        self.arguments = arguments

class Initializer(NiceRepr):

    def __init__(self, lhs, rhs):
        self.lhs = lhs
        self.rhs = rhs


