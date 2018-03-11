(ns farg.graphs
  "Functions for making, querying, and modifying graphs in FARG models."
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.core.reducers :as r]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [com.rpl.specter :as S]
            [farg.pmatch :as pmatch :refer [pmatch]]
            [farg.pgraph :as pg]
            [farg.util :as util :refer [dd dde]]
            [farg.with-state :refer [with-state]]
            [potemkin :refer [import-vars]]))

; Naming convention:
;
; (make-* g args...)  Makes an item, puts it in the graph, and returns
;                     [g item].
; (add-* g args...)   Same as make-* but only returns g.

;TODO
; Make the functions implement the above naming convention.

;;; export relevant functions from pgraph ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import-vars
  [farg.pgraph next-id pgraph has-elem? elem-type find-edgeid add-node
    add-nodes nodes edges elems has-node? attr attrs set-attr set-attrs
    has-edge? add-edge-return-id add-edge ports-of incident-edges
    incident-ports other-id neighbors-of neighboring-edges-of pprint
    transitive-closure-of-edges-to-edges remove-edge remove-node
    as-seq pgraph->edn incident-elems])

;;; placing nodes (for purposes of graph layout)

(def TAU (* 2 Math/PI))

(defn quadrant [θ]
  (let [frac (rem (/ θ TAU) 1.0)]
    (if (>= frac 0.0)
      (cond
        (<= frac 0.25) 1
        (<= frac 0.50) 2
        (<= frac 0.75) 3
        4)
      (cond
        (>= frac -0.25) 4
        (>= frac -0.50) 3
        (>= frac -0.75) 2
        1))))

(defn pt-on-ellipse
  [[cx cy] a b θ]
  (let [tanθ (Math/abs (Math/tan θ))
        denom (Math/sqrt (+ (* b b)
                            (* a a tanθ tanθ)))
        x (/ (* a b)
             denom)
        y (/ (* a b tanθ)
             denom)]
    (case (quadrant θ)
      1 [(+ cx x) (+ cy y)]
      2 [(- cx x) (+ cy y)]
      3 [(- cx x) (- cy y)]
      4 [(+ cx x) (- cy y)])))

(defn get-max-xy
  "Returns [g [maxx maxy]], the value of the :max-xy gattr of g. Sticks
  [0.0 0.0] into :max-xy as a side-effect if :max-xy is not defined in g."
  [g]
  (cond
    :let [max-xy (pg/gattr g :max-xy)]
    (some? max-xy)
      [g max-xy]
    [(pg/set-gattr g :max-xy [0.0 0.0]) [0.0 0.0]]))

(defn place-primary-node
  "Returns updated g. Assigns a hopefully reasonable value to node's :xy
  attr. Updates gattrs of g to track nodes already placed."
  [g node]
  (with-state [g g]
    (setq [maxx maxy] (get-max-xy))
    (pg/set-attr node :xy [maxx maxy])
    (pg/set-gattr :max-xy [(+ maxx 1.0) maxy])))

(def angle-range [(/ (* 6.0 Math/PI) 6.0), (/ Math/PI 6.0)])

(defn place-tag
  [g tag taggee1 taggee2]
  (let [c1 (pg/attr g taggee1 :xy)
        c2 (pg/attr g taggee2 :xy)
        c (util/midpoint c1 c2)
        a (+ (util/distance c c1) 0.1)
        b 1.0
        θ (util/choose-expr (apply util/rand angle-range)
                            (- (apply util/rand angle-range)))]
    (pg/set-attr g tag :xy (pt-on-ellipse c a b θ))))

;;; making nodes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-node-attrs [nodename]
  (cond
    (= :bind nodename)
      {:class :bind, :a 0.2, :needs-bdx? false}
    (= :succ nodename)
      {:class :tag, :a 0.2, :needs-bdx? false}
    (symbol? nodename)
      {:class :letter, :letter nodename, :a 0.2, :needs-bdx? true}))

(defn make-node
  "Returns [g id] where g is updated graph and id is the name assigned to
  the new node."
 ([g nodename]
  (make-node g nodename {}))
 ([g nodename attrs]
  (let [[g id] (pg/next-id g nodename)
        g (pg/add-node g id (merge (make-node-attrs nodename) attrs))]
    [g id])))

(defn add-tag [g tag from to]
  (with-state [g g]
    (setq t (make-node tag))
    (pg/add-edge [t :from] [from :tags])
    (pg/add-edge [t :to] [to :tags])
    (place-tag t from to)))

(defn tag? [g elem]
  (= :tag (pg/attr g elem :class)))

(defn add-binding [g from to]
  (with-state [g g]
    (setq b (make-node :bind))
    (pg/add-edge [b :from] [from :bdx])
    (pg/add-edge [b :to] [to :bdx])))

;;; shorthand ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
      (doseq [node nodes]
        (place-primary-node node))
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

(defn make-graph [elems]
  (reduce add-shorthand-elem (pgraph) elems))

(defmacro graph [& elems]
  `(make-graph ~(vec (map quote-special elems))))
