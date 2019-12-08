# criteria.py -- Criterion classes to pass to PortGraph.look_for()

class Tagged:

    def __init__(self, tagclass):
        self.tagclass = tagclass

    def __call__(self, g, nodeid):
        return g.has_tag(nodeid, self.tagclass)

class HasValue:

    def __init__(self, value):
        self.value = value

    def __call__(self, g, nodeid):
        return g.value_of(nodeid) == self.value
