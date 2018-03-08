(ns farg.graphs
  "Functions for making, querying, and modifying graphs in FARG models."
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.core.reducers :as r]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [com.rpl.specter :as S]
            [farg.pmatch :as pmatch :refer [pmatch]]
            [farg.pgraph :as g :refer [pgraph]]
            [farg.util :as util :refer [dd dde]]
            [farg.with-state :refer [with-state]]))

(defn evals-to-self? [x]
  (or (keyword? x) (number? x) (= () x)))

(defn- quote-special [shorthand-elem]
  (pmatch shorthand-elem
    ~x (guard (evals-to-self? x))
      x
    (quote ~x)
      shorthand-elem
    (adj-seq ~@elems)
      `(list '~'adj-seq ~@(map quote-special elems))))

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

(defn add-adj-seq [g nodes]
  (cond
    :let [[g m-nodes] (make-nodes-and-save-ids g nodes)]
    (empty? nodes) g
    (= 1 (count nodes)) g
    (with-state [g g]
      (doseq [[left-node right-node] (partition 2 1 nodes)]
        (bind left-id (get m-nodes left-node))
        (bind right-id  (get m-nodes right-node))
        (g/set-attr left-id :adj-right right-id)
        (g/set-attr right-id :adj-left left-id)))))

(defn- add-shorthand-elem [g shorthand-elem]
  (pmatch shorthand-elem
    (adj-seq ~@nodes)
      (add-adj-seq g nodes)
      ))

(defn make-graph [elems]
  (reduce add-shorthand-elem (pgraph) elems))

(defmacro graph [& elems]
  `(make-graph ~(vec (map quote-special elems))))
