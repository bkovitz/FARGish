# numbospec.py -- Data structures that specify the Numbo model

from FARGSpec import FARGSpec, EdgeInfo

edgeinfos = [
    EdgeInfo('taggees', 'tags'),
    EdgeInfo('support_from', 'support_to')
]

spec = FARGSpec(edgeinfos)
