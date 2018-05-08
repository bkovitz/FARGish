(ns farg.graphs3
  "Functions for making, querying, and modifying graphs in FARG models."
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.tools.macro :refer [macrolet]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [farg.pmatch :as pmatch :refer [pmatch]]
            [farg.pgraph :as pg]
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
  [farg.pgraph next-id pgraph has-elem? elem-type find-edgeid add-node
    add-nodes nodes edges elems has-node? attr attrs set-attr set-attrs
    has-edge? add-edge-return-id add-edge ports-of elem->incident-edges
    port->incident-edges incident-ports other-id neighbors-of
    neighboring-edges-of pprint transitive-closure-of-edges-to-edges
    remove-edge remove-node as-seq pgraph->edn incident-elems gattrs
    update-attr update-edge-attr merge-default-attrs edge-as-map
    pprint-nodes weight add-weight attrstr remove-elem neighbor-via
    neighbors-via edge->incident-ports other-port port->neighbor
    port->neighbors port->neighboring-ports edgestr])

(defn match-by-name? [m name]
  (let [match?f (:name-match? m)]
    (or (= (:name m) name)
        (and (some? match?f) (match?f name)))))

(defn find-class-by-elem-name [g classes-key nm]
  (or (find-first #(match-by-name? % nm)
                  (-> g :spec classes-key vals))
      {}))

(defn default-attrs [g classes-key nm]
  (if-let [cl (find-class-by-elem-name g classes-key nm)]
    (assoc (get cl :attrs {})
           :class (:name cl))
    {}))

(defn make-node
  "Returns [g id] where g is updated graph and id is the name assigned to
  the new node."
 ([g nodename]
  (make-node g nodename {}))
 ([g nodename attrs]
  (let [default-attrs (default-attrs g :nodeclasses nodename)
        [g id] (pg/next-id g nodename)
        g (pg/add-node g id (merge default-attrs attrs))]
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
        g (pg/add-edge g [id1 p1] [id2 p2] (merge default-attrs attrs))
        edgeid (pg/find-edgeid g [id1 p1] [id2 p2])]
    [g edgeid])))

(defn add-edge [& args]
  (let [[g _] (apply make-edge args)]
    g))

;;; Shorthand

(defn evals-to-self? [x]
  (or (keyword? x) (number? x) (= () x)))

(defn- quote-special [shorthand-elem]
  (pmatch shorthand-elem
    ~x (guard (evals-to-self? x))
      x
    (quote ~x)
      shorthand-elem
    (left-to-right-seq ~@elems)
      `(list '~'left-to-right-seq ~@(map quote-special elems))))

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

(defn add-left-to-right-seq [g nodes]
  (cond
    :let [[g m-nodes] (make-nodes-and-save-ids g nodes)]
    (empty? nodes) g
    (= 1 (count nodes)) g
    (with-state [g g]
      (doseq [[left-node right-node] (partition 2 1 nodes)]
        (bind left-id (get m-nodes left-node))
        (bind right-id  (get m-nodes right-node))
        (pg/set-attr left-id :adj-right right-id)
        (pg/set-attr right-id :adj-left left-id)))))

(defn- add-shorthand-elem [g shorthand-elem]
  (pmatch shorthand-elem
    (left-to-right-seq ~@nodes)
      (add-left-to-right-seq g nodes)
      ))

;;; Spec

(def empty-farg-spec
  {:type ::farg-spec
   :nodeclasses {}
   :edgeclass {}
   :stems {}})

(def empty-nodeclass
  {:name nil
   :name-match? nil ;Boolean function: if a node's name matches, make it
                    ;this class
   :attrs {}})      ;Initial attrs of any instance

(def empty-edgeclass
  {:name nil
   :name-match? nil
   :attrs {}})

(defn start-named-elem [empty-m name]
  (assoc empty-m :name name))

(defn args->map
  "args is vec of maps."
  [args]
  (with-state [m {}]
    (doseq [arg args]
      (assoc (::elem-type arg) (:arg arg)))))

(defmulti add-spec-elem (fn [spec elem] (::elem-type elem)))

(defmethod add-spec-elem :nodeclass
  [spec {:keys [name args]}]
  (update-in spec [:nodeclasses name]
    (fnil merge (start-named-elem empty-nodeclass name))
    (args->map args)))

(defmethod add-spec-elem :edgeclass
  [spec {:keys [name args]}]
  (update-in spec [:edgeclasses name]
    (fnil merge (start-named-elem empty-edgeclass name))
    (args->map args)))

(defmethod add-spec-elem :stems
  [spec {:keys [arg]}]
  (update spec :stems merge arg))

(defn make-farg-spec [elems]
  (with-state [spec empty-farg-spec]
    (doseq [elem elems]
      (add-spec-elem elem))))

(defmacro farg-spec [& elems]
  `(macrolet [(~'nodeclass [name# & args#]
                {::elem-type :nodeclass, :name name#, :args (vec args#)})
              (~'edgeclass [name# & args#]
                {::elem-type :edgeclass, :name name#, :args (vec args#)})
              (~'name-match? [arg#]
                {::elem-type :name-match?, :arg arg#})
              (~'attrs [arg#]
                {::elem-type :attrs, :arg arg#})
              (~'stems [arg#]
                {::elem-type :stems, :arg arg#})]
     (make-farg-spec ~(vec elems))))

;TODO Write this more thoughtfully
(defn merge-spec [old-spec new-spec]
  (merge-with merge old-spec (dissoc new-spec :type)))

;;; Graph constructors

;(defn make-graph [elems]
;  (reduce add-shorthand-elem (pgraph) elems))
;
;(defmacro graph [& elems]
;  `(make-graph ~(vec (map quote-special elems))))

(defn add-graph-elem [g elem]
  (cond
    (= ::farg-spec (:type elem))
      (with-state [g g]
        (update :spec #(merge-spec % elem))
        (update :stems merge (:stems (:spec g))))
    (add-node g elem)))

(defn graph [& elems]
  (with-state [g (pgraph)]
    (assoc :spec empty-farg-spec)
    (doseq [elem elems]
      (add-graph-elem elem))))
