(ns farg.layout
  "Functions to lay out a FARGish pgraph. Everything here happens in terms
  of virtual coordinates, independent of Seesaw, the screen, printing, or
  any specific output medium."
  (:refer-clojure :exclude [cond rand rand-int])
  (:require [better-cond.core :refer [cond]]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [com.rpl.specter :as S]
            [farg.pgraph :as g :refer [pgraph]]
            [farg.util :as util :refer [dd find-first]]
            [farg.with-state :refer [with-state]]))

(def empty-layout {})

;(defn update-layout [layout new-graph]
;  (

(defn gen-nodesym [[node x-or-y]]
  (cond
    :let [s (str node x-or-y)]
    (Character/isDigit (first s))
      (gensym (str "num" s))
    (gensym s)))

(defn implication-mates [g node]
  (set (keep #(g/attr g node %) [:adj-right :adj-left])))

(defn implication-components [g]
  (loop [nodes (g/nodes g), components #{}]
    (cond
      (empty? nodes)
        components
      :let [node (first nodes), nodes (rest nodes)
            mates (implication-mates g node)]
      (empty? mates)
        (recur nodes components)
      :let [mate-components (filter #(not (empty?
                                      (clojure.set/intersection % mates)))
                                    components)]
      (empty? mate-components)
        (recur nodes (conj components #{node}))
      :let [new-component (-> (apply clojure.set/union mate-components)
                              (conj node))]
      (recur nodes
             (conj (clojure.set/difference components mate-components)
                   new-component)))))

(defn placed? [g node]
  (some? (g/attr g node :xy)))

(defn place-by-implication
  "Returns updated g. node is already-placed node. relation is a keyword
  indicating which relation mate bears to node. mate is the node to place."
  [g node relation mate]
  (let [[x y] (g/attr g node :xy)]
    (case relation
      :adj-right
        (g/set-attr g mate :xy [(+ x 1.0) y])
      :adj-left
        (g/set-attr g mate :xy [(- x 1.0) y]))))

;(defn lay-out [g]
;  (let [nodes (->> g g/nodes (sort-by str))
;        node-xys (mapcat #(vector [% 'x] [% 'y]) nodes)
;        syms (map gen-nodesym node-xys)
;        node-xy->sym (into {} (map vector node-xys syms))]
;    ;NEXT Make equations, solve equations
;    (pprint node-xy->sym)))

(defn- spread-implications [g icomp]
  (loop [g g, node (find-first #(placed? g %) icomp), icomp (disj icomp node)]
    (cond
      (empty? icomp)
        g
      :let [[g mates] (place-mates g node)]
      (recur g (clojure.set/difference icomp mates)))))

(defn- spread-implications [g icomp]
  (loop [g g
         nodes-to-spread-from #{(find-first #(placed g %) icomp)}
         nodes-done #{}]
    (cond
      (empty? nodes-to-spread-from)
        g
      :let [node (first nodes-to-spread-from)
            nodes-to-spread-from (disj nodes-to-spread-from node)
            relations (
            mate-infos (
      ;NEXT place-by-implication  Place only unplaced nodes
      


(defn- add-implications
  "Returns g updated with xys filled in by implication. icomp is an
  implication component: a set of nodes whose :xy attrs imply each other."
  [g icomp]
  (cond
    (every? #(placed? g %) icomp)
      g
    (not-any? #(placed? g %) icomp)
      (fresh-implications g icomp)
    (spread-implications g icomp)))

(defn lay-out [g]
  (reduce add-implications g (implication-components g)))
