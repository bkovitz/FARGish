# Predefs.py -- Standard predefined symbols in FARGish

from util import as_iter


def Tagged(g, tagclass, *nodeids):
    '''Are all the nodeids tagged with tagclass? If nodeids is empty, returns
    False. Each nodeid can be a nodeid or an iterable of of nodeids.'''
    if not nodeids:
        return False
    for n in nodeids:
        for nodeid in as_iter(n):
            if not g.has_tag(nodeid, tagclass):
                return False
    return True

def AllTagged(g, tagclass, nodeids):
    '''Same as Tagged(), but takes an iterable.'''
    return Tagged(g, tagclass, *nodeids)

def Not(x):
    return not x

