# PortGraph.py -- PortGraph class

from watcher import Watcher, Response
from util import nice_object_repr, repr_str, as_iter, is_iter, reseed, \
        sample_without_replacement, intersection, empty_set
from exc import TooManyArgs0, TooManyArgs
from BuildSpec import make_buildspec
from NodeParams import NodeParams

import networkx as nx

from collections import defaultdict, namedtuple, UserDict
from collections.abc import Iterable
from inspect import isclass
from operator import attrgetter, itemgetter
from random import choice, choices
from io import StringIO
import traceback


empty_node_params = NodeParams()

class Node:

    can_need_update = False  # override with True to get NeedsUpdate tag
    min_support_for = 0.0
    initial_support_for = 0.01
    node_params = None       # a NodeParams object

    @classmethod
    def args_into_kwargs(cls, args, kwargs):
        '''kwargs will not be modified.'''
        if cls.node_params is None:
            return kwargs.copy()
        else:
            return cls.node_params.args_into_kwargs(args, kwargs)

    @classmethod
    def make_filled_params(cls, g, args, kwargs):
        node_params = cls.node_params
        if node_params is None:
            node_params = empty_node_params
        return node_params.make_filled_params(
            g, cls.args_into_kwargs(args, kwargs)
        )
        
    #TODO rm?
    @classmethod
    def exactly_matches_kwargs(cls, g, node, kwargs):
        '''Would building a node with this class and arguments kwargs result
        in a duplicate of 'node'?'''
        if g.class_of(node) != cls:
            return False
        if cls.node_params is None:
            return True # SMALL BUG Shouldn't we check node attrs?
        else:
            return cls.node_params.is_exact_match(g, node, kwargs)

    def __init__(self, *args, **kwargs):
        if self.node_params is not None:
            try:
                kwargs = self.node_params.args_into_kwargs(args, kwargs)
            except TooManyArgs0 as exc:
                num_args = len(exc.args)
                raise TooManyArgs(
f'''{self.__class__.__name__}: More arguments ({len(exc.args)}) than parameters ({len(self.node_params)}): {repr(exc.args)}.'''
                )
            self.node_params.on_init(self, kwargs)
        else:
            #TODO What about *args?
            pass
        self.kwargs = kwargs
#        for k, v in kwargs.items():
#            setattr(self, k, v)
        #TODO Redesign so that attributes passed in kwargs can't name-clash
        #with other attributes of Node, like the methods.

    #TODO rm
    def on_build(self, g, thisid):
#        if self.node_params is not None:
#            self.node_params.on_build(g, thisid, self.kwargs)
        pass

    #TODO rm; replaced by .on_build()
#    def auto_link(self, thisid, g):
#        '''Creates links to mates, if any; should be called immediately upon
#        building the node.'''
#        pass

    def is_attrs_match(self, other):
        return True

    def __repr__(self):
#        exclude = set(['kwargs', 'id'])  #, 'member_of'])
#        #TODO Ignore self.link_specs; show items other than MateParams
#        if isinstance(self.link_specs, Iterable):
#            exclude |= set(ls.new_node_port_label for ls in self.link_specs)
#        if self.node_params:
#            exclude |= self.node_params.exclude_from_node_repr()
#        kvs = [kv for kv in self.__dict__.items()
#                      if kv[0] not in exclude]

        if self.node_params:
            return repr_str(
                self.__class__.__name__,
                self.node_params.node_repr_kvs(self)
            )
        else:
            return self.__class__.__name__

    def datumstr(self, g, node):
        return repr(self)  # Override to exploit g

    def display_name(self, g, node):
        return self.__class__.__name__

    def __eq__(self, other):
        return repr(self) == repr(other)

    def __hash__(self):
        return hash(repr(self))

    def __getattr__(self, name):
        '''All attrs default to None, to make them easy to override in
        subclasses.'''
        return None

    def update_support(self, g, node):
        pass

    #TODO rm; Fail tags should be managed by scouts, etc.
    def fail(self, g, node):
        pass


class Tag(Node):

    tag_port_label = 'taggees'
    taggee_port_label = 'tags'
    mutual_support = True
    is_tag = True

    @classmethod
    def add_tag(cls, g, taggees):
        '''Builds an instance of the tag in g and attaches it to taggee at
        the appropriate ports. Tag is a member of all of the taggees'
        containers. Returns tag_id. taggees can be a single
        node id or an iterable of node ids.'''
        #TODO More general way to specify taggees; maybe a dict.
        #TODO Don't build the tag if the taggees already have this tag.
        taggees = list(as_iter(taggees))
        taggee_containers = intersection(
            *[g.member_of(ee) for ee in as_iter(taggees)]
        )
        tag = g.make_node(cls, container=taggee_containers)
        for taggee in as_iter(taggees):
            g.add_edge(tag, cls.tag_port_label, taggee, cls.taggee_port_label,
                       mutual_support=cls.mutual_support)
        return tag

    def datumstr(self, g, node):
        return '%s(%s)' % (self.__class__.__name__, ', '.join(
            g.nodestr(ee) for ee in g.taggees_of(node)
        ))


class NeedsUpdate(Tag, Watcher):

    def look(self, g, node, nodes=None):
        return [UpdateSupport(node)]


class UpdateSupport(Response):

    def __init__(self, node):
        self.node = node

    def go(self, g):
        g.update_support(self.node)


class CouldMake(Tag):

    def __init__(self, bindings):
        self.bindings = bindings  #TODO I think this is obsolete (BEN)


HopBase = namedtuple('Hop',
    ['from_node', 'from_port_label', 'to_node', 'to_port_label', 'key']
)
class Hop(HopBase):
    def reverse(self):
        return Hop(
            self.to_node,
            self.to_port_label,
            self.from_node,
            self.from_port_label,
            self.key
        )

class HopDict:

    def __init__(self):
        self.d_from_port_label = {}  # from_port_label: set(Hop)
        self.d_to_node = {}          # to_node: set(Hop)

    def all_hops(self):
        '''Returns a set of hops.'''
        result = set()
        for hset in self.d_from_port_label.values():
            result = result.union(hset)
        return result

    def from_port_labels(self):
        return self.d_from_port_label.keys()

    def add(self, hop):
        try:
            self.d_from_port_label[hop.from_port_label].add(hop)
        except KeyError:
            self.d_from_port_label[hop.from_port_label] = set([hop])
        try:
            self.d_to_node[hop.to_node].add(hop)
        except KeyError:
            self.d_to_node[hop.to_node] = set([hop])

    def remove(self, hop):
        '''It is not an error to remove a hop that doesn't exist.'''
        try:
            self.d_from_port_label[hop.from_port_label].discard(hop)
        except KeyError:
            pass
        try:
            self.d_to_node[hop.to_node].discard(hop)
        except KeyError:
            pass

    def remove_all_hops_to(self, to_node):
        '''It is not an error if there are no hops to to_node.'''
        for hop in list(self.hops_to_neighbor(to_node)):
            self.remove(hop)

    def hops_from_port_label(self, from_port_label):
        '''Returns a set of Hops.'''
        try:
            return self.d_from_port_label[from_port_label]
        except KeyError:
            return empty_set

    def hops_to_port_label(self, to_port_label):
        '''Returns a seq of hops.'''
        return (hop for hop in self.all_hops()
                       if hop.to_port_label == to_port_label)

    def hops_to_neighbor(self, neighbor_node):
        '''Returns a set of Hops.'''
        try:
            return self.d_to_node[neighbor_node]
        except KeyError:
            return empty_set


class NodeAttrDict(UserDict):
    '''Custom dict that maps nodes in a Graph to their attributes. Every
    node automatically gets an attribute named '_hops' whose value is a
    defaultdict(set).'''

    def __setitem__(self, node, node_attrs):
        '''node_attrs must be a dictionary object.'''
        super().__setitem__(node, node_attrs)
        self.data[node]['_hops'] = HopDict()


# Convenience class for holding a node's id along with the value stored in 
# its datum.
NodeAndValue = namedtuple('NodeAndValue', ['node', 'value'])


#TODO Rename as WeightedNodes. Pass a weight function to __init__.
class NodesWithSalience:
    '''Each element in 'nodes' can be either a nodeid or a collection of
    nodeids.'''

    def __init__(self, g=None, nodes=[], multiplier=None):
        self.nodes = []
        self.weights = []
        if g:
            for node in nodes:
                if is_iter(node):
                    salience = sum(g.salience(n) for n in node)
                else:
                    salience = g.salience(node)
                if callable(multiplier):
                    salience *= multiplier(g, node)
                self.add(node, salience)
        else:
            if nodes is not None:
                raise ValueError('NodesWithSalience: nodes provided to constructor without a graph; must provide graph in order to determine salience.')

    def add(self, node, salience):
        if salience > 0.0:
            self.nodes.append(node)
            self.weights.append(salience)

    def choose(self, k=1):
        '''Returns a generator of k nodes chosen randomly, weighted by
        salience.'''
        return sample_without_replacement(
            self.nodes, k=k, weights=self.weights
        )

    def choose1(self):
        '''Returns a randomly chosen node, or None if there are no nodes
        in the NodesWithSalience.'''
        lis = list(self.choose(k=1))
        if lis:
            return lis[0]
        else:
            None

    def __len__(self):
        return len(self.nodes)

    def __repr__(self):
        if not self.nodes:
            return 'NodesWithSalience()'
        fmt = '  %%%ds  %%.3f' % max(len(str(node)) for node in self.nodes)
        return 'NodesWithSalience(\n%s\n)' % '\n'.join(
            (fmt % tup)
                for tup in sorted(
                    zip(self.nodes, self.weights), key=itemgetter(1)
                )
        )


class PortGraph(nx.MultiGraph):

    node_dict_factory = NodeAttrDict

    default_salience = 0.01
    port_mates = None  # Override in subclass to specify default PortMates

    def __init__(self, *args, **kwargs):
        # In kwargs,
        # 'port_mates' = PortMates object to deduce port labels for auto-linking
        kws = kwargs.copy()
        kws['seed'] = reseed(kws.get('seed', None))
        if self.port_mates and 'port_mates' not in kws:
            kws['port_mates'] = self.port_mates
        #print('KWS', kws)
        super().__init__(*args, **kws)
        self.nextid = 1
        self.during_touch = False
        self.touched_nodes = set()
        self.new_nodes = set()
        self.after_touch_nodes = set()
        self.builder = None  #HACK: For auto-linking new nodes to current "builder"

    def _bump_nextid(self):
        result = self.nextid
        self.nextid += 1
        return result

    def mknode(self, o):
        '''Innermost function for making nodes in a FARG model. Most callers
        should call .make_node() instead.

        Builds a new node and sets its datum. If o is a class, we call it
        with no arguments and set the datum to the constructed object.
        Otherwise we set the datum to o. We set the salience according
        to o.default_salience (from the object, not the class). Returns the
        new node's id.'''
        i = self._bump_nextid()
        if isclass(o):
            o = o()
        self.add_node(i, **{'datum': o})
        if self.builder is not None:
            self.add_edge(i, 'builder', self.builder, 'built')
        try:
            salience = o.default_salience
            if salience is not None:
                self.set_salience(i, salience)
            self.set_support_for(
                i,
                max(o.initial_support_for, o.min_support_for)
            )
            if callable(o.after_touch_update):
                self.after_touch_nodes.add(i)
        except AttributeError:
            pass
        self.new_nodes.add(i)
        return i

    def add_edge_to_default_container(self, nodeid):
        '''If nodeid has no member_of link, add link to the "lowest common
        denominator" member_of all its neighbors.'''
        if self.has_neighbor_at(nodeid, 'member_of'):
            return
        containers = intersection(*(
            self.neighbors(n1, 'member_of') for n1 in self.neighbors(nodeid)
        ))
        for c in containers:
            self.add_edge(nodeid, 'member_of', c, 'members')

    def make_node(g, nodeclass, *args, **kwargs):
        '''Builds a new node with specified class and arguments, fills
        the node's datum with specified attrs, gives the node an 'id' attr
        holding its id, and links the node to specified mates--unless a node
        linked in the exact same neighbors already exists. Returns the nodeid
        if created; otherwise None.'''
        spec = make_buildspec(g, nodeclass, args=args, kwargs=kwargs)
        return spec.build(g)
        
    def dup_node(self, h, node):
        'h is another PortGraph, which must contain node.'
        attrs = h.nodes[node]
        new_attrs = dict((k, attrs[k]) for k in ['_class', 'value']
                                           if k in attrs)
        return self.make_node(new_attrs)

    def already_built(self, cl, args=None, kwargs={}):
        '''Is a node of class cl with the given args and kwargs already built?'''
        buildspec = make_buildspec(self, cl, args=args, kwargs=kwargs)
        result = buildspec.already_built(self)
        #print('ALR', cl, result)
        return result

    def candidate_nodes_wsal(self, nodeclass=None, exclude=None, nodes=None):
        '''"Candidate nodes with salience." Candidate nodes to consider for
        searching or choosing from.  Future version should consider focal point
        and maybe salience.  In current version, simply returns all the nodes
        of nodeclass except 'exclude'. Either argument may be omitted.  Returns
        a NodesWithSalience.'''
        if nodeclass:
            nodes = self.nodes_of_class(nodeclass, nodes=nodes)
        if nodes is None:
            nodes = self.nodes
        nodes = set.difference(set(nodes), as_iter(exclude))
        return NodesWithSalience(self, nodes)

    def nodestr(self, node):
        return str(node) + ': ' + self.datumstr(node)

    def datumstr(self, node):
        datum = self.datum(node)
        if datum is None:
            return '(no datum)'
        else:
            return datum.datumstr(self, node)
        #return str(self.datum(node))

    def _add_edge(self, node1, port_label1, node2, port_label2, **attr):
        '''If the edge already exists, doesn't make a new one. Regardless,
        returns the key of the edge. Calls boost_salience on node1 and
        node2. If the edge did not already exist, we "touch" node1 and node2.'''
        #TODO Probably get rid of the mutual_support flag.
        mutual_support = attr.get('mutual_support', False)
        try:
            del attr['mutual_support']
        except KeyError:
            pass
        hop = self.find_hop(node1, port_label1, node2, port_label2)
        if hop:
            key = hop.key
            if 'weight' in attr:  # HACK: What about other attrs?
                self[node1][node2][key]['weight'] = attr['weight']
        else:
            key = super(PortGraph, self).add_edge(node1, node2, **attr)
            hop1 = Hop(node1, port_label1, node2, port_label2, key)
            hop2 = Hop(node2, port_label2, node1, port_label1, key)
            self.nodes[node1]['_hops'].add(hop1)
            self.nodes[node2]['_hops'].add(hop2)
            self.touch(node1)
            self.touch(node2)
        if mutual_support:
            self.add_mutual_support(node1, node2)
        return key

    def add_edge(self, node1, port_label1, node2, port_label2, **attr):
        '''If the edge already exists, doesn't make a new one. Regardless,
        returns the key of the edge. Calls boost_salience on node1 and
        node2. If the edge did not already exist, we "touch" node1 and node2.
        node1 and node2 can be nodeids, lists, or None.'''
        for n1 in as_iter(node1):
            for n2 in as_iter(node2):
                self._add_edge(n1, port_label1, n2, port_label2, **attr)

    def has_edge(self, u, v, w=None, y=None):
        if y is None:
            return super().has_edge(u, v, w)
        else:
            node1 = u
            node1_port_label = v
            node2 = w
            node2_port_label = y
            return (
                self.has_hop(node1, node1_port_label, node2, node2_port_label)
                or  # Is the 'or' necessary? Is overriding has_edge() necessary,
                    # since we have has_hop()?
                self.has_hop(node2, node2_port_label, node1, node1_port_label)
            )

    def remove_edge(self, node1, port_label1, node2, port_label2):
        '''It is not an error to remove an edge that does not exist. If the
        edge did exist, we remove it and "touch" both nodes.'''
        hop = self.find_hop(node1, port_label1, node2, port_label2)
        if hop:
            self.remove_hop(hop)
            self.touch(node1)
            self.touch(node2)

    def remove_hop(self, hop_or_hops):
        #Unlike remove_edge, remove_hop doesn't touch the nodes
        for hop in as_iter(hop_or_hops):
            node1 = hop.from_node
            node2 = hop.to_node
            self.nodes[node1]['_hops'].remove(hop)
            self.nodes[node2]['_hops'].remove(hop.reverse())
            super().remove_edge(node1, node2, hop.key)
        
    def remove_node(self, node_or_nodes):
        for node in as_iter(node_or_nodes):
            self._remove_all_hops_to(node)
            super().remove_node(node)

    def remove_nodes_from(self, nodes):
        nodes = list(nodes) # We iterate over nodes twice.
        for node in nodes:
            self._remove_all_hops_to(node)
        super().remove_nodes_from(nodes)

    def _remove_all_hops_to(self, node):
        for neighbor in list(self.neighbors(node)):
            self.hopdict(neighbor).remove_all_hops_to(node)

    def hops_from_node(self, node):
        return self.nodes[node]['_hops'].all_hops()

    def hops_from_port(self, node, port_label):
        try:
            return self.nodes[node]['_hops'].hops_from_port_label(port_label)
        except KeyError:
            return []

    def hops_to_neighbor(self, node, neighbor_node):
        return (
            self.nodes[node]['_hops'].hops_to_neighbor(neighbor_node)
        )

    def port_labels(self, node):
        #TODO Look at node's NodeParams?
        return self.nodes[node]['_hops'].from_port_labels()

    def is_port_label(self, name):
        try:
            port_mates = self.graph['port_mates']
        except KeyError:
            return False
        return port_mates.is_port_label(name)

    def auto_link(self, from_node, port_label, to_node):
        try:
            port_mates = self.graph['port_mates']
        except KeyError:
            return
        port_mates.auto_link(self, from_node, port_label, to_node)

    def find_hop(self, from_node, from_port_label, to_node, to_port_label):
        '''Returns the Hop if it exists, else None.'''
        try:
            return next(hop for hop in self.hops_to_neighbor(from_node, to_node)
                                if hop.from_port_label == from_port_label
                                    and
                                    hop.to_port_label == to_port_label)
        except StopIteration:
            return None

    def edge_weight(self, edge):
        'edge is a tuple (from_node, to_node, key). weight defaults to 0.0.'
        try:
            return self.edges[edge]['weight']
        except KeyError:
            return 0.0

    def hop_weight(self, *args):
        '''0.0 if hop does not exist. 1.0 if hop exists but has no weight
        specified.'''
        if len(args) == 4:
            from_node, from_port_label, to_node, to_port_label = args
            hop = self.find_hop(
                from_node, from_port_label, to_node, to_port_label
            )
        elif len(args) == 1:
            hop = args[0]
        else:
            raise ValueError('Illegal args for hop_weight: %s' % repr(args))
        if hop is None:
            return 0.0
        return self[hop.from_node][hop.to_node][hop.key].get('weight', 1.0)

    def edge_to_hop(self, edge):
        'edge is a tuple (from_node, to_node, key). Returns a Hop or None.'
        from_node, to_node, key = edge
        try:
            return next(hop for hop in self.hops_to_neighbor(from_node, to_node)
                            if hop.key == key)
        except StopIteration:
            return None

    def all_hops(self):
        'Returns a generator of all hops in self.'
        return (self.edge_to_hop(edge) for edge in self.edges)

    def has_hop(self, from_node, from_port_label, to_node, to_port_label):
        return bool(
            self.find_hop(from_node, from_port_label, to_node, to_port_label)
        )

    def touch(self, node):
        '''Adds node to set of 'touched' nodes. Call do_touches() to touch
        them.'''
        self.touched_nodes.add(node)

    def do_touch(self, node):
        if not self.during_touch:
            self.during_touch = True
            s = self.salience(node) #DEBUG
            self.boost_salience(node)
            #print('DO_TOUCH %50s  %.3f  %.3f' % (
            #    self.nodestr(node), s, self.salience(node)
            #))
            if self.can_need_update(node):
                self.add_tag(NeedsUpdate, node)
            self.during_touch = False

    def do_touches(self):
        '''Calls all follow-ups for all touched nodes, including
        after_touch_nodes, and then clears .touched_nodes and .new_nodes.'''
        for node in self.touched_nodes:
            self.do_touch(node)
        for tn in list(self.after_touch_nodes):
            d = self.datum(tn)
            if d:
                d.after_touch_update(
                    self, tn, self.touched_nodes, self.new_nodes
                )
            else:
                self.after_touch_nodes.remove(tn)
        self.touched_nodes.clear()
        self.new_nodes.clear()

    def can_need_update(self, node):
        datum = self.datum(node)
        if datum:
            return datum.can_need_update
        else:
            return False

    def OLDadd_tag(
        self, tag_or_tagclass, node_or_nodes,
        tag_port_label='taggees', node_port_label='tags'
    ):
        '''Links a tag to one or more nodes. Returns the tag's id.
        If tag_or_tagclass is a class, builds the tag.
        The tag supports its taggees.'''
        any_exist = any(self.has_node(n) for n in as_iter(node_or_nodes))
        if not any_exist:
            return

        #TODO Allow other options regarding support (e.g. less support,
        #opposition, reciprocal support).
        if isclass(tag_or_tagclass) or isinstance(tag_or_tagclass, Node):
            # Make the tag node
            tag = self.make_node(
                tag_or_tagclass,
                container=self.common_container(node_or_nodes)
            )
        else:
            # We'll link from an existing tag node
            tag = tag_or_tagclass
        # Link the tag to the taggee(s)
        for node in as_iter(node_or_nodes):
            self.add_edge(tag, tag_port_label, node, node_port_label)
            self.add_support(tag, node)
        return tag

    def add_tag(
        self, tag_or_tagclass, node_or_nodes,
        tag_port_label='taggees', node_port_label='tags'
    ):
        '''Links a tag to one or more nodes. Returns the tag's id.
        If tag_or_tagclass is a class, builds the tag.
        The tag supports its taggees.'''
        return self.make_node(
            tag_or_tagclass,
            **{tag_port_label: node_or_nodes}
        )

    def replace_tag(
        self, node_or_nodes, old_tag_or_tagclass, new_tag_or_tagclass,
        tag_port_label='taggees', node_port_label='tags'
    ):
        '''Removes old_tag_or_tagclass, adds new_tag_or_tagclass to
        node_or_nodes.'''
        self.remove_tag(node_or_nodes, old_tag_or_tagclass)
        self.add_tag(tag_or_tagclass=new_tag_or_tagclass,
                     node_or_nodes=node_or_nodes,
                     tag_port_label=tag_port_label,
                     node_port_label=node_port_label)

    #TODO UT
    def move_tag(self, tagclass, fromids, toids):
        '''Moves all tags of class tagclass from fromids to toids. fromids and
        toids can be either a single integer or a list of integer node ids.
        If none of the fromids have such a tag, does nothing.'''
        if self.remove_tag(fromids, tagclass):
            for toid in as_iter(toids):
                self.add_tag(tagclass, toid)

    def has_tag(self, node, tagclass, taggee_port_label='tags'):
        '''Returns True iff node has at least one tag of class tagclass.
        tagclass can be an integer, specifying the nodeid of a specific
        tag node; if so, returns True iff node is linked to that node at
        taggee_port_label.'''
        if isclass(tagclass):
            try:
                return any(
                    tag for tag in self.neighbors(
                            node,
                            port_label=taggee_port_label
                        )
                            if issubclass(self.class_of(tag), tagclass)
                )
            except KeyError:
                return False
        else:
            return tagclass in self.neighbors(
                node, port_label=taggee_port_label
            )

    def all_have_tag(self, tagclass, nodes, taggee_port_label='tags'):
        return all(
            self.has_tag(node, tagclass, taggee_port_label=taggee_port_label)
                for node in nodes
        )

    def all_share_tag(self, tagclass, nodes, taggee_port_label='tags'):
        return bool(self.tags_of(
            nodes, tagclass=tagclass, taggee_port_label=taggee_port_label
        ))

    def tags_of(self, nodes, tagclass=Tag, taggee_port_label='tags'):
        '''Returns a generator. nodes can be a single node id or an iterable
        of node ids.'''
        #TODO Should be able to specify tagclass more narrowly.
        tag_sets = (
            set(tag
                for node in as_iter(nodes)
                    for tag in self.neighbors(
                            node, port_label=taggee_port_label
                        )
                        if self.is_of_class(tag, tagclass)
            )
        )
        return set.intersection(tag_sets)

    def tag_of(self, node, tagclass=Tag, taggee_port_label='tags'):
        #TODO Handle multiple taggees, different port labels
        try:
            return next(iter(self.tags_of(
                node, tagclass=tagclass, taggee_port_label=taggee_port_label
            )))
        except StopIteration:
            return None
        
    #TODO Consistent argument order: put tag_or_tagclass first?
    def remove_tag(self, node_or_nodes, tag_or_tagclass):
        '''Removes all tags of node that match tagclass. Returns True iff at
        last one of node_or_nodes was actually so tagged.'''
        #TODO Should only remove the edge if the tag tags other nodes, too.
        #TODO Should be able to specify the tagclass more narrowly (e.g.
        # by value, by other taggees).
        result = False
        if isclass(tag_or_tagclass):
            tagids = list(self.tags_of(node_or_nodes, tagclass=tag_or_tagclass))
            if tagids:
                self.remove_nodes_from(tagids)
            result = True
        else:
            self.remove_node(tag_or_tagclass)
            result = True # HACK BUG We should check that node_or_nodes really
                          # has the given tag node.
        return result
        
    #TODO UT port_label as iterable
    def neighbors(
        self,
        node,
        port_label=None,
        neighbor_class=None,
        nbr_label=None
    ):
        '''Returns a set. If neighbor_class is not None, returns only
        neighbors of that class. If port_label is None, returns all
        neighbors at all of node's ports. If port_label is an iterable,
        returns the union of all the neighbors at the specified ports.
        If nbr_label is not None, returns only neighbors playing the role
        nbr_label in relation to node. node can be a single nodeid or an
        iterable of nodeids.'''
        if port_label is None:
            #result = super().neighbors(node)
            result = []
            for node in as_iter(node):
                result += super().neighbors(node)
        elif node is None:
            result = []
        else:
            result = set()
            for pl in as_iter(port_label):
                for node in as_iter(node):
                    result.update(
                        hop.to_node
                            for hop in self.hops_from_port(node, pl)
                    )
        if neighbor_class:
            result = set(
                n for n in result if self.is_of_class(n, neighbor_class)
            )
        # INEFFICIENT
        if nbr_label:
            result = set(
                n for n in result if self.is_in_role(n, nbr_label)
            )

        return result

    def neighbor(self, node, port_label=None):
        '''Returns 'first' neighbor of node, at optional port_label. If there
        is more than one neighbor, the choice is made arbitrarily. If there
        is no neighbor, returns None.'''
        try:
            return next(iter(self.neighbors(node, port_label=port_label)))
        except StopIteration:
            return None

    def has_neighbor_at(self, node, port_label, neighbor_class=None):
        '''Does node have a neighbor of neighbor_class linked to port_label?
        If neighbor_class is None, then any class of neighbor will do.'''
        if neighbor_class is None:
            return bool(self.neighbor(node, port_label))
        else:
            return any(self.is_of_class(neighbor, neighbor_class)
                          for neighbor in self.neighbors(node, port_label))

    def taggees_of(self, tag, port_label='taggees'):
        return self.neighbors(tag, port_label=port_label)

    def taggee_of(self, tag, port_label='taggees'):
        try:
            return next(iter(self.taggees_of(tag, port_label=port_label)))
        except StopIteration:
            return None

    def taggee_value(self, tag, port_label='taggees'):
        return self.value_of(self.taggee_of(tag, port_label=port_label))

    def add_member_edge(self, group_node, member_node):
        self.add_edge(group_node, 'members', member_node, 'member_of')

    def members_of(self, group_node):
        return list(self.neighbors(group_node, 'members'))

    def members_to_subgraph(self, group_node):
        return self.subgraph(self.members_of(group_node))

    def member_of(self, node):
        return self.neighbors(node, port_label='member_of')

    def container_of(self, node):
        '''Like member_of, but returns member_of node's taggees if node is a
        tag and is not a member of anything.'''
        ms = self.member_of(node)
        if not ms:
            ms = set()
            for taggee in self.taggees_of(node):
                ms.update(self.member_of(taggee))
        return ms

    def common_container(self, node_or_nodes):
        '''Returns the one container that all of node_or_nodes are a member_of,
        or None if they lack a single common container or have more than one
        common container.'''
        containers = intersection(
            *(self.member_of(n) for n in as_iter(node_or_nodes))
        )
        if len(containers) == 1:
            return list(containers)[0]
        else:
            return None

    #TODO UT
    def members_recursive(self, group_node):
        result = set()
        visited = set()
        to_visit = set([group_node])
        while to_visit:
            members = set()
            for node in to_visit:
                for m in self.members_of(node):
                    members.add(m)
            result |= members
            visited |= to_visit
            to_visit = members - visited
        return result

    def is_member(self, group_node, node):
        return self.has_hop(group_node, 'members', node, 'member_of')

    def reset_hops_from_port(
        self, from_nodes, from_port_label, to_nodes, to_port_label
    ):
        to_nodes = set(as_iter(to_nodes))
        for from_node in as_iter(from_nodes):
            for to_node in as_iter(to_nodes):
                self.add_edge(
                    from_node, from_port_label, to_node, to_port_label
                )
            for hop in self.hops_from_port(from_node, from_port_label):
                if hop.to_node not in to_nodes:
                    self.remove_hop(hop)

    def hopdict(self, node):
        return self.nodes[node]['_hops']

    def class_of(self, node):
        '''Returns None is node does not exist or lacks a datum.'''
        datum = self.datum(node)
        if datum is None:
            return None
        else:
            return self.datum(node).__class__

    def is_of_class(self, node, cl):
        for c in as_iter(cl):
            if not isclass(c):
                c = c.__class__
            try:
                if issubclass(self.class_of(node), c):
                    return True
            except TypeError:
                continue
        return False

    def label_is_a(self, label, ancestor_label):
        '''Is label the same as or a descendent of ancestor_label?'''
        #TODO Implement port-label inheritance. This version just tests
        #equality.
        return label == ancestor_label

    def value_of(self, node, attr_name='value'):
        try:
            #v = self.datum(node).value
            v = getattr(self.datum(node), attr_name)
        except AttributeError:
            return None
#        if v is None and self.is_of_class(node, Tag):
#            return self.value_of(self.taggee_of(node))
        return v

    def have_same_value(self, node1, node2):
        '''A value of None is not considered the same as anything, even another
        None.'''
        v1 = self.value_of(node1)
        if v1 is None:
            return False
        v2 = self.value_of(node2)
        if v2 is None:
            return False
        return v1 == v2

    def datum(self, node):
        '''Returns the datum associated with node. The datum is presumed to be
        an object that inherits from Node. If node does not exist or lacks
        a datum, returns None.'''
        try:
            return self.nodes[node]['datum']
        except KeyError:
            return None

    def all_datums(self):
        '''Returns a list of all the datums, i.e. a datum for each node in the
        graph.'''
        return [self.datum(id) for id in self.nodes]

    def call_method(self, nodeid, method_name, *args, **kwargs):
        '''Returns result of calling method method_name(self, nodeid) on nodeid
        if nodeid exists and its datum has a callable attr named method_name.
        Otherwise returns None.'''
        d = self.datum(nodeid)
        if d is None:
            return None
        m = getattr(d, method_name)
        if callable(m):
            return m(self, nodeid, *args, **kwargs)
        else:
            return None

    def display_name(self, nodeid):
        return self.call_method(nodeid, 'display_name')

    def raw_salience(self, node):
        '''Returns node's salience. If no salience has been set explicitly
        for node, returns default_salience. If node does not exist,
        returns 0.0.'''
        try:
            return self.nodes[node].get('salience', self.default_salience)
        except KeyError:
            return 0.0

    def salience(self, node):
        #HACK
        '''Returns sum of node's raw salience and node's support.'''
        return self.raw_salience(node) + self.support_for(node)

    def set_salience(self, node, salience):
        '''Sets raw salience.'''
        try:
            self.nodes[node]['salience'] = salience
        except KeyError:
            pass

    def boost_salience(
        self, node, new_salience=None, multiplier=1.1, min_boost=0.1
    ):
        old_salience = self.raw_salience(node)
        if new_salience is None:
            new_salience = max(
                old_salience + min_boost,
                self.raw_salience(node) * multiplier
            )
        self.set_salience(node, new_salience)

    def gross_boost_salience(self, node, addend=1.0):
        self.set_salience(node, self.raw_salience(node) + addend)

    def decay_saliences(self):
        for node in self.nodes:
            self.decay_salience(node)

    def decay_salience(self, node):
        self.set_salience(node, 0.9 * self.raw_salience(node))

    #TODO mv to WithSupport mix-in
    def support_for(self, node):
        try:
            return self.nodes[node]['support']
        except KeyError:
            return 0.0

    def min_support_for(self, node):
        try:
            return self.datum(node).min_support_for
        except AttributeError:
            return 0.0

    #TODO mv to WithSupport mix-in
    def set_support_for(self, node, support):
        self.nodes[node]['support'] = support

    def add_support(self, node, neighbor, weight=0.2):
        #print('ADD_S', node, neighbor, weight)
        if weight is None:
            self.add_edge(node, 'support_to', neighbor, 'support_from')
        else:
            self.add_edge(node, 'support_to', neighbor, 'support_from',
                **{'weight': weight})

    def oppose(self, node, neighbor, weight=-0.1):
        self.add_support(node, neighbor, weight=weight)

    def add_mutual_opposition(self, node1, node2, weight=-0.1):
        self.oppose(node1, node2, weight=weight)
        self.oppose(node2, node1, weight=weight)

    def add_mutual_support(self, node, neighbor, weight=0.2):
        self.add_support(node, neighbor, weight=weight)
        self.add_support(neighbor, node, weight=weight)
        #if self.datum(neighbor).gives_reciprocal_support: #HACK
        #self.add_edge(neighbor, 'support_to', node, 'support_from')

    def remove_mutual_support(self, node, neighbor):
        self.remove_edge(node, 'support_to', neighbor, 'support_from')
        self.remove_edge(neighbor, 'support_to', node, 'support_from')

    def update_support(self, node):
        datum = self.datum(node)
        if datum is not None:
            datum.update_support(self, node)

    def update_all_support(self):
        for node in self.nodes:
            self.update_support(node)

    def supports(self, from_node, to_node):
        return self.has_edge(from_node, 'support_to', to_node, 'support_from')

    def update(self, node_or_nodes):
        for nodeid in as_iter(node_or_nodes):
            d = self.datum(nodeid)
            if d is None:
                continue
            u = d.update
            if callable(u):
                u(self, nodeid)

    def find_member_in_role(self, group_node, role):
        #TODO UT
        '''role is a port label that a neighbor of the sought node must have.
        Returns either a node playing that role in relation to another member
        of group_node, or None. If more than one node fits the criterion, the
        choice is arbitrary.'''
        for member in self.members_of(group_node):
            for hop in self.hopdict(member).hops_to_port_label(role):
                if self.is_member(group_node, hop.to_node):
                    return member
        return None

    def look_for(self, *criteria):
        '''Returns one node that meets criteria, or None if not found.
        criteria are functions that take two arguments: g, nodeid, and
        return a true value if nodeid matches the criterion.'''
        nodes = self.nodes() # Start with all nodes (INEFFICIENT)
        for c in criteria:
            nodes = [n for n in nodes if c(self, n)]
            if not nodes:
                return False
        return choice(nodes) # TODO choose by salience?

    def nodes_of_class(self, cl, nodes=None):
        #TODO UT
        result = []
        if nodes is None:
            nodes = self.nodes
        for node in nodes:
            datum = self.nodes[node]['datum']
            if issubclass(datum.__class__, cl):
                result.append(node)
        return result

    def node_of_class(self, cl, nodes=None):
        '''Returns a nodeid of a node of class cl, selected from nodes (all
        nodes in graph if None), or None if no such node exists. If there is
        more than one node that meets the conditions, the choice is made
        arbitrarily. Typically you should only call this when you are sure
        that only one such node exists.'''
        #TODO UT
        if nodes is None:
            nodes = self.nodes
        for node in nodes:
            if self.is_of_class(node, cl):
                return node
        return None

    def nodes_with_tag(self, tagclass, nodes=None, taggee_port_label='tags'):
        'Returns a generator of nodes that have a tag of class tagclass.'
        if nodes is None:
            nodes = self.nodes
        return (
            node for node in nodes
                     if self.has_tag(
                         node,
                         tagclass,
                         taggee_port_label=taggee_port_label
                     )
        )

    def nodes_without_tag(self, tagclass, nodes=None, taggee_port_label='tags'):
        'Returns a generator of nodes that do not have a tag of class tagclass.'
        if nodes is None:
            nodes = self.nodes
        return (
            node for node in nodes
                     if not self.has_tag(
                         node,
                         tagclass,
                         taggee_port_label=taggee_port_label
                     )
        )

    def nodes_matching_datum(self, datum, nodes=None):
        if nodes is None:
            nodes = self.nodes
        cl = datum.__class__
        for node in nodes:
            node_datum = self.datum(node)
            if (issubclass(node_datum.__class__, cl)
                and
                datum.is_attrs_match(node_datum)
               ):
                yield node

    def partition_nodes(self, nodeids, gpred):
        '''Returns a tuple of lists: (those satisfying gpred, those not).
        gpred takes two arguments: g, nodeid.'''
        yes = []
        no = []
        for nodeid in nodeids:
            if gpred(self, nodeid):
                yes.append(nodeid)
            else:
                no.append(nodeid)
        return yes, no

    def choose_by_salience(self, nodes, k=1):
        return choices(
            list(nodes), k=k, weights=[self.salience(n) for n in nodes]
        )

    def scope_of(self, node):
        '''Returns set containing the viewees of the node's neighbors at its
        'scope' port.'''
        scope_nodes = self.neighbors(node, port_label='scope')
        result = set()
        for scope_node in scope_nodes:
            result.update(self.neighbors(scope_node, port_label='viewees'))
        return result

    def is_in_role(self, node, role):
        'role is the port label of a neighbor of node.'
        return any(self.hopdict(node).hops_to_port_label(role))

class ValueOf:
    '''Function that returns the value of a nodeid in graph g. Returns None
    if g.value_of(nodeid) returns None.'''

    def __init__(self, g):
        self.g = g

    def __call__(self, nodeid):
        return self.g.value_of(nodeid)

#    @classmethod
#    def is_attrs_match(cls, node_attrs, host_node_attrs):
#        try:
#            return (
#                node_attrs['value'] == host_node_attrs['value']
#            )
#        except KeyError:
#            return False


#def is_node_match(tg, target_node, hg, host_node):
#    try:
#        target_attrs = tg.nodes[target_node]
#        host_attrs = hg.nodes[host_node]
#        #target_class = target_attrs['_class']
#        #host_class = host_attrs['_class']
#        target_class = target_attrs['datum'].__class__
#        host_class = host_attrs['datum'].__class__
#        return (
#            issubclass(host_class, target_class)
#            and
#            target_class.is_attrs_match(target_attrs, host_attrs)
#        )
#    except KeyError:
#        return False

def is_node_match(bg, base_node, hg, host_node):
    try:
        base_datum = bg.nodes[base_node]['datum']
        host_datum = hg.nodes[host_node]['datum']
    except KeyError:
        return False
    return (
        issubclass(host_datum.__class__, base_datum.__class__)
        and
        base_datum.is_attrs_match(host_datum)
    )

def is_port_label_match(target_port_label, host_port_label):
    if (
        isinstance(target_port_label, str)
        or
        isinstance(target_port_label, numbers.Number)
    ):
        return target_port_label == host_port_label
    else:
        return issubclass(host_port_label, target_port_label)

def pt(g):
    if 't' in g.graph:
        print('t=' + str(g.graph['t']))

def pn(g):
    '''Prints the nodes in the graph'''
    pt(g)
    for node in g.nodes:
        print(g.nodestr(node))

def long_nodestr(g, node):
    #TODO Correct behavior if node does not exist
    sio = StringIO()
    print('%s\n\n  support=%.3f\n  salience=%.3f\n' % (
        g.nodestr(node),
        g.support_for(node),
        g.salience(node)
    ), file=sio)
    for hop in sorted(
        g.hops_from_node(node), key=attrgetter('from_port_label')
    ):
        print('  %s --> %s %s (%.3f)' % (
            hop.from_port_label,
            g.nodestr(hop.to_node),
            hop.to_port_label,
            g.hop_weight(hop)
        ), file=sio)
    return sio.getvalue()

def pg(g, nodes=None):
    '''Prints graph g in simple text form.'''
    pt(g)
    if nodes is None:
        nodes = g.nodes
    elif isclass(nodes) and issubclass(nodes, Node):
        nodes = g.nodes_of_class(nodes)
    for node in as_iter(nodes):
        print('%s  supp=%.3f sal=%.3f' % (
            g.nodestr(node),
            g.support_for(node),
            g.salience(node)
        ))
        for hop in sorted(
            g.hops_from_node(node), key=attrgetter('from_port_label')
        ):
            print('  %s --> %s %s (%.3f)' % (
                hop.from_port_label,
                g.nodestr(hop.to_node),
                hop.to_port_label,
                g.hop_weight(hop)
            ))

def ps(g, nodes=None, by='support', e=False):
    '''Prints each node with its support and salience (but not its edges).
    e=True to see support edges.'''
    pt(g)
    if nodes is None:
        nodes = g.nodes
    elif isclass(nodes):
        nodes = g.nodes_of_class(nodes)

    if by == 'support':
        key = lambda node: g.support_for(node)
    elif by == 'id':
        key = lambda node: node
    elif by == 'salience':
        key = lambda node: g.raw_salience(node)
    elif callable(by):
        key = by
    else:
        raise ValueError('invalid argument for by: %s' % repr(by))
    for node in sorted(as_iter(nodes), key=key):
        print('supp=%.3f rawsal=%.3f  %s' % (
            g.support_for(node),
            g.raw_salience(node),
            g.nodestr(node)
        ))
        if e:
            s_froms = list(g.neighbors(node, port_label='support_from'))
            #TODO Separate support from opposition
            if s_froms:
                print('  <-- %s' % ', '.join(g.nodestr(n) for n in s_froms))
            s_tos = list(g.neighbors(node, port_label='support_to'))
            if s_tos:
                print('  --> %s' % ', '.join(g.nodestr(n) for n in s_tos))


if __name__ == '__main__':
    g = PortGraph()
    g.add_edge('A', 'in', 'B', 'out')
    g.add_edge('O', 'members', 'A', 'member_of')
    #print(g.nodes(data=True))
    print(list(g.neighbors('A')))
    print(list(g.neighbors('A', 'in')))
    print(list(g.neighbors('B', 'out')))
    print(g.hops_to_neighbor('A', 'B'))
#    g.remove_edge('A', 'in', 'B', 'out')
#    print(list(g.neighbors('A')))
#    print(list(g.neighbors('A', 'in')))
#    print(list(g.neighbors('B', 'out')))

    #print(g.adj)
    #print(list(g.adjacency()))
    #g.remove_edge('A', 'in', 'B', 'out')
