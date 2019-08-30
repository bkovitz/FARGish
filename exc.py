# exc.py -- Custom exceptions for FARGish

from util import nice_object_repr


class FargDone(Exception):

    def done_msg(self):
        return 'FargDone' # This should be overridden

class NumboSuccess(FargDone):
    def __init__(self, g, target):
        'g must be a NumboGraph.'
        self.g = g
        self.target = target

    def done_msg(self):
        return 'Success!  ' + str(self.g.expr_as_equation(self.target))

    __repr__ = nice_object_repr

class FargCantRespond(Exception):
    pass
