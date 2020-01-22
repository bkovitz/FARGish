# numbo3a.py -- Manually "compiled" FARGish for brute-force numble solver

from bases import ActiveNode

class Want(ActiveNode):
    '''A node that wants a node to exist with the same value as that of
    self.targetid.'''

    def __init__(self, targetid):
        self.targetid = targetid

    def actions(self, g, thisid):
        return [
            maybeBuildScout(g, OperandsScout, behalf_of=thisid),
            maybeBuildScout(
                g, BacktrackingScout, behalf_of=thisid, targetid=targetid),
            maybeBuildScout(g, DoneScout, behalf_of=thisid, targetid=targetid)
        ]

# TODO Better: Build.maybe_make (already in numbospec.py)
def maybeBuildScout(g, scout_class, **kwargs):
    if not scout_already_exists(g, scout_class, kwargs):
        return Build(

class Scout(ActiveNode):
    '''A Scout is a node that is looking for something on behalf of some other
    node. When the Scout finds it, it does something.'''

    def __init__(self, **kwargs):
        self.kwargs = kwargs
        try:
            setattr(self, 'behalf_of', kwargs['behalf_of'])
        except KeyError:
            raise(ValueError(
                f'{self.__class__.__name__} ctor missing behalf_of.'
            ))

class OperandsScout(Scout):
    '''A Scout that searches for pairs of Avail Numbers and tries to combine
    them with an arithmetic Operator.'''

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
