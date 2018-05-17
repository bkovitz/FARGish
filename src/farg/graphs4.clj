(ns farg.graphs4
  "Functions for making, querying, and modifying graphs in FARG models."
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.core.strint :refer [<<]]
            [clojure.math.combinatorics :as combo]
            [clojure.tools.macro :refer [macrolet symbol-macrolet]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [com.rpl.specter :as S]
            [farg.pmatch :as pmatch :refer [pmatch]]
            [farg.pgraph2 :as pg]
            [farg.util :as util :refer [dd find-first]]
            [farg.with-state :refer [with-state]]
            [potemkin :refer [import-vars]]))

; Naming convention:
;
; (make-* g args...)  Makes an item, puts it in the graph, and returns
;                     [g item].
; (add-* g args...)   Same as make-* but only returns g.

;;; export relevant functions from pgraph ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import-vars
  [farg.pgraph2 next-id pgraph has-elem? elem->type find-edgeid
    nodes edges elems has-node? attr attrs set-attr set-attrs
    has-edge? elem->incident-edges elem->neighbor? user-attrs
    port->incident-edges edge->incident-ports other-id elem->neighbors
    pprint transitive-closure-of-edges-to-edges
    remove-edge remove-node #_as-seq #_pgraph->edn edge->incident-elems gattrs
    update-attr #_update-edge-attr edge-as-map
    pprint-nodes attrstr remove-elem
    edge->incident-ports other-port
    port->neighbors port->neighboring-ports edgestr])

(defn port->neighbor [g port]
  (first (port->neighbors g port)))

(defn weight [g id]
  (cond
    :let [w (pg/attr g id :weight)]
    (nil? w)
      0.0
    w))

(defn add-weight [g id delta]
  (pg/set-attr g id :weight (+ delta (weight g id))))

(defn merge-default-attrs
 ([g id default-attrs]
  (pg/set-attrs g id (merge default-attrs (pg/attrs g id))))
 ([g id default-attrs1 default-attrs2 & more]
  (merge-default-attrs g id (apply merge default-attrs1 default-attrs2 more))))

(defn match-by-name? [m name]
  (let [match?f (:name-match? m)]
    (or (= (:name m) name)
        (and (some? match?f) (match?f name)))))

(defn find-class-by-elem-name
  "Returns nil if can't find it."
  [g classes-key nm]
  (find-first #(match-by-name? % nm)
              (-> g :spec classes-key vals)))

(defn default-attrs [g classes-key nm]
  (if-let [cl (find-class-by-elem-name g classes-key nm)]
    (assoc (get cl :attrs {})
           :class (:name cl))
    {}))

(defn class-of [g id]
  (pg/attr g id :class))

(defn classdef-of
 ([g id]
  (case (pg/elem->type g id)
    ::pg/node
      (get-in g [:spec :nodeclasses (class-of g id)])
    ::pg/edge
      (get-in g [:spec :edgeclasses (class-of g id)])
    nil))
 ([g id k]
  (get (classdef-of g id) k)))

;;; Current context (for shorthand)

(def empty-current-ctx
  {:type ::current-ctx, ::ctx nil, ::members #{}, ::convenient-names #{}})

(defn current-convenient-names [g]
  (S/select-one [::current-ctx S/FIRST ::convenient-names] g))

(defn push-ctx [g ctxid]
  (update g ::current-ctx conj
            (assoc empty-current-ctx
                   ::ctx ctxid
                   ::convenient-names (current-convenient-names g))))

(defn has-current-ctx? [g]
  (not (empty? (S/select-one ::current-ctx g))))

(defn add-member-to-current-ctx [g memberid]
  (if (has-current-ctx? g)
    (S/transform [::current-ctx S/FIRST ::members] #(conj % memberid) g)
    g))

(defn members-of-current-ctx [g]
  (S/select-one [::current-ctx S/FIRST ::members] g))

(defn pop-ctx [g]
  (update g ::current-ctx rest))

;;; Auto node attrs (for shorthand)

(defn push-auto-node-attrs [g attrs]
  (S/transform ::auto-node-attrs #(conj % (merge (first %) attrs)) g))

(defn auto-node-attrs [g]
  (S/select-one [::auto-node-attrs S/FIRST] g))

(defn pop-auto-node-attrs [g]
  (S/transform ::auto-node-attrs rest g))

;;; Convenient names (for shorthand)

(defn save-convenient-name [g name id]
  (if (has-current-ctx? g)
    (S/setval [::current-ctx S/FIRST ::convenient-names name] id g)
    g))

(defn look-up-name [g name]
  (cond
    :let [foundid (S/select-one
                    [::current-ctx S/FIRST ::convenient-names name] g)]
    (some? foundid)
      foundid
    (has-elem? g name)
      name
    nil))

;;; Access to spec

(defn can-link? [g [id1 pl1] [id2 pl2]]
  (contains? (-> g :spec :can-link) (hash-set pl1 pl2)))

(defn port-label-isa? [g child ancestor]
  (isa? (get-in g [:spec :portclass-hierarchy]) child ancestor))

(defn ports-from-classdef 
  "Returns lazy seq of ports [id port-label] derived from id's class definition
  in (:spec g)."
  [g id]
  (->> (S/select-one [:spec :nodeclasses (class-of g id) :port-labels] g)
       (map (fn [port-label] [id port-label]))))

;;; pgraph overrides

(defn elem->ports [g id]
  (clojure.set/union
    (set (pg/elem->ports g id))
    (set (ports-from-classdef g id))))
;TODO Also override elem->port-labels

;(defn elem->port-labels [g id]

(defn +suffix [nm as]
  (cond
    :let [suffix (get as ::suffix)]
    (nil? suffix)
      nm
    (keyword? nm)
      (keyword (str (name nm) suffix))
    (symbol? nm)
      (symbol (str (name nm) suffix))
    (string? nm)
      (str nm suffix)
    (fn? suffix)
      (suffix nm)
    nm))

(defn make-node
  "Returns [g id] where g is updated graph and id is the name assigned to
  the new node."
 ([g nodename]
  (make-node g nodename {}))
 ([g nodename attrs]
  (let [default-attrs (default-attrs g :nodeclasses nodename)
        auto-attrs (auto-node-attrs g)
        as (merge default-attrs auto-attrs attrs)
        [g id] (pg/make-node g (+suffix nodename as) as)
        g (add-member-to-current-ctx g id)]
    [g id])))

(defn add-node [& args]
  (let [[g _] (apply make-node args)]
    g))

(defn make-edge
  "Returns [g id] where g is updated graph and id is the name assigned to
  the new node."
 ([g edgename [id1 p1] [id2 p2]]
  (make-edge g edgename [id1 p1] [id2 p2] {}))
 ([g edgename [id1 p1] [id2 p2] attrs]
  (let [default-attrs (default-attrs g :edgeclasses edgename)
        [g edgeid] (pg/make-edge g edgename [id1 p1] [id2 p2]
                                 (merge default-attrs attrs))
        g (add-member-to-current-ctx g edgeid)]
    [g edgeid])))

(defn add-edge [& args]
  (let [[g _] (apply make-edge args)]
    g))

;;; Spec

(def empty-farg-spec
  {:type ::farg-spec
   :nodeclasses {}
   :edgeclass {}
   :can-link #{} ;Each elem is a set of two port labels (or one to indicate
                 ;that a port label can link to itself)
   :portclass-hierarchy (make-hierarchy)
   :stems {}})

(def empty-nodeclass
  {:name nil
   :name-match? nil ;Boolean function: if a node's name matches, make it
                    ;this class
   :attrs {}        ;Initial attrs of any instance
   :port-labels #{}})

(def empty-edgeclass
  {:name nil
   :name-match? nil
   :attrs {}})

(def empty-portclass
  {:name nil
   :extends []})

(defn start-named-elem [empty-m name]
  (assoc empty-m :name name))

(defn args->map
  "args is vec of maps, each of which has an ::elem-type."
  [args]
  (with-state [m {}]
    (doseq [arg args]
      (assoc (::elem-type arg) (cond
                                 (contains? arg :arg)
                                   (:arg arg)
                                 (contains? arg :args)
                                   (:args arg))))))

(defn merge-spec-arg [old new]
  (if (coll? old)
    (into old new)
    new))

(defn merge-args [m-class m-args]
  (merge-with merge-spec-arg m-class m-args))

(defmulti add-spec-elem (fn [spec elem] (::elem-type elem)))

(defmethod add-spec-elem :nodeclass
  [spec {:keys [name args]}]
  (update-in spec [:nodeclasses name]
    (fnil merge-args (start-named-elem empty-nodeclass name))
    (args->map args)))

(defmethod add-spec-elem :edgeclass
  [spec {:keys [name args]}]
  (update-in spec [:edgeclasses name]
    (fnil merge-args (start-named-elem empty-edgeclass name))
    (args->map args)))

(defmethod add-spec-elem :portclass
  [spec {:keys [name args]}]
  (update-in spec [:portclasses name]
    (fnil merge-args (start-named-elem empty-portclass name))
    (args->map args)))

(defmethod add-spec-elem :can-link
  [spec {:keys [portclasses]}]
  (update spec :can-link conj portclasses))

(defmethod add-spec-elem :stems
  [spec {:keys [arg]}]
  (update spec :stems merge arg))

(defn safe-derive [h child parent]
  (if (isa? h child parent)
    h
    (derive h child parent)))

(defn mk-hierarchy [classes]
  (reduce (fn [h {:keys [name extends]}]
            (reduce #(safe-derive %1 name %2) h extends))
          (make-hierarchy)
          classes))

(defn make-farg-spec [elems]
  (with-state [spec empty-farg-spec]
    (doseq [elem elems]
      (add-spec-elem elem))
    (assoc :portclass-hierarchy (mk-hierarchy (vals (:portclasses spec))))))
      ;TODO More inheritance

(defmacro farg-spec [& elems]
  `(macrolet [(~'nodeclass [name# & args#]
                {::elem-type :nodeclass, :name name#, :args (vec args#)})
              (~'edgeclass [name# & args#]
                {::elem-type :edgeclass, :name name#, :args (vec args#)})
              (~'portclass [name# & args#]
                {::elem-type :portclass, :name name#, :args (vec args#)})
              (~'name-match? [arg#]
                {::elem-type :name-match?, :arg arg#})
              (~'attrs [arg#]
                {::elem-type :attrs, :arg arg#})
              (~'port-labels [& args#]
                {::elem-type :port-labels, :args (apply hash-set args#)})
              (~'can-link [port-label1# port-label2#]
                {::elem-type :can-link
                 :portclasses #{port-label1# port-label2#}})
              (~'extends [& args#]
                {::elem-type :extends, :args (vec args#)})
              (~'stems [arg#]
                {::elem-type :stems, :arg arg#})]
     (make-farg-spec ~(vec elems))))

;TODO Write this more thoughtfully
(defn merge-spec [old-spec new-spec]
  (-> (merge-with merge old-spec (dissoc new-spec :type :can-link))
      (update :can-link clojure.set/union (:can-link new-spec))))

;;; Shorthand

(defn make-nodes-and-save-ids
  "Returns [g' m-nodes], where g' is the graph g updated with newly created
  nodes, and m-nodes is a map {node node-id} containing the ids that the
  new nodes were assigned."
  [g nodes]
  (reduce (fn [[g m] node]
            (let [[g nodeid] (make-node g node)]
              [g (assoc m node nodeid)]))
            [g {}]
          nodes))

(defmulti add-graph-elem (fn [g elem] (or (::elem-type elem)
                                          (:type elem)
                                          (class elem))))

(defmethod add-graph-elem ::farg-spec
  [g spec]
  (with-state [g g]
    (update :spec #(merge-spec % spec))
    (update :stems merge (:stems (:spec g)))))

(defmethod add-graph-elem ::left-to-right-seq
  [g {:keys [args]}]
  (cond
    :let [nodes args
          [g m-nodes] (make-nodes-and-save-ids g nodes)]
    (empty? nodes) g
    (= 1 (count nodes)) g
    (with-state [g g]
      (doseq [[left-node right-node] (partition 2 1 nodes)]
        (bind left-id (get m-nodes left-node))
        (bind right-id  (get m-nodes right-node))
        (pg/set-attr left-id :adj-right right-id)
        (pg/set-attr right-id :adj-left left-id)))))

(defn ensure-endpoint
  "Returns [g id] where id is id of elem referred to by nm. Creates a node
  named after nm if one doesn't exist."
  [g nm]
  (if (has-elem? g nm)
    [g nm]
    (make-node g nm)))

(defn linkable-ports
  "Returns seq of [[fromid port-label1] [toid port-label2]] ..."
  [g fromid toid]
  (->> (combo/cartesian-product
         (->> (elem->ports g fromid)
              (filter #(port-label-isa? g (second %) :out)))
         (->> (elem->ports g toid)
              (filter #(port-label-isa? g (second %) :in))))
       (filter #(apply can-link? g %))))

(defn unique-linkable-ports
  "Returns [[fromid port-label1] [toid port-label2]]. Throws exception if
  there are no ports available to link between fromid and toid, or if
  there is not a single, unique pair."
  [g fromid toid]
  (cond
    :let [pairs (linkable-ports g fromid toid)]
    (= 1 (count pairs))
      (first pairs)
    (empty? pairs)
      (throw (IllegalArgumentException. (<< "No ports of ~{fromid} and ~{toid} "
        "can link to each other.")))
    (throw (IllegalArgumentException. (<< "Multiple pairs of ports of "
      "~(pr-str fromid) and ~(pr-str toid) can link to each other: "
      "~(pr-str pairs).")))))

(defn throw-odd-kvs [v]
  (throw (IllegalArgumentException. (<< "Vector for specifying a node "
    "must have an even number of arguments (key-value pairs) after "
    "the name of the node: ~{v}."))))

(defn throw-no-such-elem [name v]
  (throw (IllegalArgumentException.
    (<< "No node or edge named ~{name}: ~{v}."))))

(defmethod add-graph-elem clojure.lang.PersistentVector
  [g v]
  (pmatch v
    []
      (add-node g [])
    [~from ->]
      (throw (IllegalArgumentException. (<< v ": need endpoint for edge.")))
    [~from -> ~to] ;TODO attrs
      (cond
        :let [fromid (look-up-name g from)]
        (nil? fromid)
          (throw-no-such-elem from v)
        :let [toid (look-up-name g to)]
        (nil? toid)
          (throw-no-such-elem to v)
        (with-state [g g]
          (bind [from-port to-port] (unique-linkable-ports g fromid toid))
          (add-edge nil from-port to-port)))
    [~nodeclass ~k ~va ~@kvs]
      (if (even? (count kvs))
        (with-state [g g]
          (setq id (make-node nodeclass (-> (apply hash-map k va kvs)
                                            (assoc ::suffix va))))
          (save-convenient-name va id)
          (save-convenient-name nodeclass id))
        (throw-odd-kvs v))
    [~nodeclass]
      (let [[g id] (make-node g nodeclass)]
        (save-convenient-name g nodeclass id))
    ~nodeclass
      (let [[g id] (make-node g nodeclass)]
        (save-convenient-name g nodeclass id))))

(defmethod add-graph-elem ::ctx
  [g {:keys [name args]}]
  (with-state [g g]
    (setq ctxid (make-node name))
    (push-ctx ctxid)
    (doseq [elem args]
      (add-graph-elem elem))
    (doseq [memberid (members-of-current-ctx g)]
      (add-edge nil [ctxid :ctx-members] [memberid :ctx]))
    (pop-ctx)))

(defmethod add-graph-elem ::with-node-attrs
  [g {:keys [attrs args]}]
  (with-state [g g]
    (push-auto-node-attrs attrs)
    (doseq [elem args]
      (add-graph-elem elem))
    (pop-auto-node-attrs)))

(defmethod add-graph-elem :default
  [g elem]
  (if (and (map? elem) (contains? elem ::elem-type))
    (throw (IllegalArgumentException.
             (<< "Unknown graph element type ~(pr-str (::elem-type elem))")))
    (let [[g id] (make-node g elem)]
      (save-convenient-name g elem id))
    #_(add-node g elem)))

;;; Graph constructors

(defn make-graph [elems]
  (with-state [g (pgraph)]
    (assoc :spec empty-farg-spec
           ::auto-node-attrs (list {}))
    (push-ctx :graph)
    (doseq [elem elems]
      (add-graph-elem elem))
    (pop-ctx)))

(defmacro graph
  "We redefine -> inside the scope of graph. If you want the normal ->,
  you'll need to refer to it qualified by a namespace or by some other
  name that you define. You could always write another graph constructor,
  which defines different macros and then calls make-graph."
  [& elems]
  `(macrolet [(~'left-to-right-seq [& args#]
                {::elem-type ::left-to-right-seq, :args (vec args#)})
              (~'ctx [name# & args#]
                {::elem-type ::ctx, :name name#, :args (vec args#)})
              (~'with-node-attrs [attrs# & args#]
                {::elem-type ::with-node-attrs, :attrs attrs#,
                 :args (vec args#)})]
     (symbol-macrolet [~'-> '~'->]
       (make-graph ~(vec elems)))))
