# numbospec.py -- Data structures that specify the Numbo model

from FARGSpec import FARGSpec, EdgeInfo

edgeinfos = [
    EdgeInfo('taggees', 'tags'),
    EdgeInfo('support_from', 'support_to', clas='Support'),
    EdgeInfo('members', 'member_of', clas='Member'),
    EdgeInfo('viewing', 'view', clas='View')
]

spec = FARGSpec(edgeinfos)
