# numbospec.py -- Data structures that specify the Numbo model

from FARGSpec import FARGSpec, EdgeInfo

edgeinfos = [
    EdgeInfo('taggees', 'tags', clas='Tag'),
    EdgeInfo('support_from', 'support_to', clas='Support'),
    EdgeInfo('members', 'member_of', clas='Member'),
    EdgeInfo('viewing', 'view', clas='View'),
    EdgeInfo('agent_for', 'agents', clas='Agent')
]

spec = FARGSpec(edgeinfos)
