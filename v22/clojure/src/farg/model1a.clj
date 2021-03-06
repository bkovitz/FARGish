(ns farg.model1a
  "Abandoned version of farg.model1.

  Very simple FARG model: just make internal bindings in 'abc'."
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [com.rpl.specter :as S]
            [farg.logging :as log :refer [with-logging log logdd]]
            [farg.gui :as gui]
            [farg.graphs2 :as g :refer [graph make-graph]]
            [farg.no-reload :refer [gui-state]]
            [farg.pmatch :refer [pmatch]]
            [farg.util :as util :refer [dd remove= find-first with-rng-seed]]
            [farg.with-state :refer [with-state]]))

; TODO
; demo
; start-model
; do-timestep
; pprint
; seek-desiderata  DONE
; start-bdx  DONE
; support-desiderata  DONE
; oppose-inconsistencies  
; post-support  DONE
; post-antipathy
; normalize-support
; update-total-support

;(def default-node-attrs
;  {:total-support 1.0
;   :self-support '(permanent 1.0)

;(defn add-desiderata [fm]
;  (with-state [fm fm]
;    (doseq [node (g/nodes fm)]
;      (when (= :letter (attr fm node :class))
;        (set-attr node :desiderata 

;;; Utility functions

(defn close-to-zero? [x]
  (< (Math/abs x) 0.001))

(defn all-nodes-other-than [fm id]
  (->> (g/nodes fm) (remove= id)))

(defn class-of [fm id]
  (g/attr fm id :class))

;;; Initialization

(def letter-attrs
  {:class :letter
   :self-support `(:permanent 1.0)
   :desiderata #{:want-bdx :want-adj-bdx}})
;TODO :want-bdx-from, not :want-bdx

(def abc
  (-> (graph (left-to-right-seq 'a 'b 'c))
      (g/merge-default-attrs 'a letter-attrs {:letter 'a})
      (g/merge-default-attrs 'b letter-attrs {:letter 'b})
      (g/merge-default-attrs 'c letter-attrs {:letter 'c})
      ))

(defn start-model [fm]
  (with-state [fm fm]
    (update :stems assoc :bind 0 :support 0)
    (assoc :timestep 0
           :actions #{}
           :support-actions [])
    ))

;;; Bindings

(defn bdx? [fm id]
  (= :bind (class-of fm id)))

(defn all-bdx [fm]
  (->> fm g/edges (filter #(bdx? fm %))))

(defn add-bdx [fm from to]
  (g/add-edge fm [from :bdx-from] [to :bdx-to]
              {:class :bind
               :desiderata #{:want-mates-to-exist}
               :self-support `(:decaying 0.2)}))

(defn start-bdx-for
  "Creates bindings to id from all other nodes, and from id to all other
  nodes."
  [fm0 id]
  (with-state [fm fm0]
    (doseq [that-elem (all-nodes-other-than fm0 id)]
      (add-bdx id that-elem)
      (add-bdx that-elem id))))

(defn has-bdx?
  "Are any bindings, even if unofficial, linked to [id :bdx-from] or
  [id :bdx-to]?"
  [fm id]
  (or (seq (g/port->incident-edges fm [id :bdx-from]))
      (seq (g/port->incident-edges fm [id :bdx-to]))))

(defn incident-bdx [fm id]
  (concat
    (g/port->incident-edges fm [id :bdx-from])
    (g/port->incident-edges fm [id :bdx-to])))

(defn edge-to-adjacent? [fm id edgeid]
  (let [mate (g/other-id fm id edgeid)]
    (or (= mate (g/attr fm id :adj-right))
        (= mate (g/attr fm id :adj-left)))))

;;; Support for desiderata

;(defn boost-support [fm from to]
;  (with-state [fm fm]
;    (when (not (g/has-edge? fm [from :support-to] [to :support-from]
;    ;Create edge if it doesn't exist
;    ;Boost its weight
;    ;Maybe rethink this
;    ;Where do we do positive feedback?

(defn desideratum->support
  "Returns a seq of support actions, or nil if none."
  [fm id desideratum]
  (case desideratum
    :want-mates-to-exist  ; assumes that id is an edge
      (for [elem (g/incident-elems fm id)]
        `(:boost-support ~id ~elem))
    :want-bdx
      (for [elem (incident-bdx fm id)]
        `(:boost-support ~id ~elem))
    :want-adj-bdx
      (for [elem (->> (incident-bdx fm id)
                      (filter #(edge-to-adjacent? fm id %)))]
        `(:boost-support ~id ~elem))))

;(defn do-support-target [fm support-target]
;  (pmatch support-target
;    (:support-incident-elems ~id)
;      (with-state [fm]
;        (doseq [elem (g/incident-elems fm id)]
;          (boost-support id elem)))
;    (:support-incident-bdx ~id)
;      fm ;TODO
;    ))

(defn post-support-for-desiderata [fm]
  (with-state [fm fm]
    (doseq [id (g/elems fm)
            desideratum (g/attr fm id :desiderata)]
      (update :support-actions into (desideratum->support fm id desideratum)))))

;;; Updating support weights

(defn support? [fm id]
  (= :support (class-of fm id)))

(defn all-support-edges [fm]
  (filter #(support? fm %) (g/edges fm)))

(defn add-support-target
  "Returns updated nested map {support-src-id {support-dst-id target-amt}}."
  [m target]
  (pmatch target
    (:boost-support ~fromid ~toid)
      (cond
        :let [subm (get m fromid)]
        (nil? subm)
          (assoc m fromid {toid 1.0})
        :let [old-amt (get subm toid)]
        (nil? old-amt)
          (assoc-in m [fromid toid] 1.0)
        (update-in m [fromid toid] + 1.0))))

(defn total-support-for [fm id]
  (cond
    :let [s (g/attr fm id :total-support)]
    (nil? s) 0.0
    s))

;NEXT Figure out how to give pos feedback so it drowns out the losers.
;The current version tends to equalize support rather than exaggerating
;differences.
;SOLUTION: Have deltas, not target values. We want a single winner to clearly
;emerge, not a stable distribution among many supportees.
;
;For each elem that can give support:
; Give support to each desideratum.
;
; Give pos feedback (for existing support) to each thing that I already
; support.
;
; Normalize so that outgoing support <= 1.0. (In later versions, each group
; of competitors at one edge will have to be normalized separately.)
(defn pos-feedback-for-support [fm [fromid m0]]
  (letfn [(upd [m1 [toid target-amt]]
            (assoc m1 toid (+ target-amt (total-support-for fm toid))))]
    [fromid (reduce upd {} m0)]))
  
(defn normalize-support-targets
  "Scales target-amts (the vals of m) so they don't sum to > 1.0."
  [[fromid m]]
  (cond
    :let [total (reduce + (vals m))]
    (< total 1.0)
      [fromid m]
    [fromid (zipmap (keys m) (->> (vals m) (map #(/ % total))))]))

(defn support-map
  "Returns map {fromid {toid weight}} of existing support in fm."
  [fm]
  (with-state [m {}]
    (doseq [edgeid (all-support-edges fm)]
      (bind emap (g/edge-as-map fm edgeid))
      (assoc-in [(:support-from emap) (:support-to emap)] (:weight emap)))))

(defn remove-support-edge [fm fromid toid]
  (g/remove-edge fm [fromid :support-from] [toid :support-to]))

(defn add-support-edge [fm fromid toid weight]
  (if (close-to-zero? weight)
    (remove-support-edge fm fromid toid)
    (g/add-edge fm [fromid :support-from] [toid :support-to]
                   {:class :support, :weight weight})))

(defn make-support-targets
  "Returns seq of pairs [support-fromid {support-toid target-amt}] where
  target-amt is the amount of support that support-fromid would like to give
  support-toid based on the accumulated :support-actions in fm."
  [fm]
  (->> (:support-actions fm)
       (reduce add-support-target {})
       (map #(pos-feedback-for-support fm %))
       (dd)
       (map normalize-support-targets)))

(defn zero-support-for-abandoned-mates [target-map m-old]
  (with-state [target-map target-map]
    (doseq [old-mate (keys m-old)]
      (when (not (contains? target-map old-mate))
        (assoc old-mate 0.0)))))

(defn update-support-weight [fm fromid toid new-weight old-weight]
  (add-support-edge fm fromid toid
    (if (> new-weight 0.0)
      (min new-weight (max 0.2 (+ old-weight 0.2)))
      (max new-weight (min -0.2 (- old-weight 0.2))))))
      
(defn update-support-weights [fm]
  (let [m-old-support (support-map fm)
        m-support-targets (make-support-targets fm)
        _ (logdd :support m-old-support m-support-targets)]
    (with-state [fm fm]
      (doseq [[fromid target-map] m-support-targets]
        (bind m-old (get m-old-support fromid))
        (bind target-map (zero-support-for-abandoned-mates target-map m-old))
        (doseq [[toid new-weight] target-map]
          (update-support-weight
            fromid toid new-weight (get m-old toid 0.0)))))))

;;; Total support

(defn set-total-support-to-self-support [fm elem]
  (pmatch (g/attr fm elem :self-support)
    (:permanent ~n)
      (g/set-attr fm elem :total-support n)
    (:decaying ~n)
      (-> fm
          (g/set-attr elem :total-support n)
          (g/set-attr elem :self-support `(:decaying ~(* 0.9 n))))
    nil
      (g/set-attr fm elem :total-support 0.0)))

(defn update-total-support [fm0]
  (with-state [fm fm0]
    ;self-support
    (doseq [elem (g/elems fm0)]
      (when (not (support? fm0 elem))
        (set-total-support-to-self-support elem)))
    ;support received through :support edges
    (doseq [[fromid m] (support-map fm0)
            [toid weight] m]
      (bind from-support (total-support-for fm0 fromid))
      (g/update-attr toid :total-support + (* weight (min 1.0 from-support))))))

;;; Actions

(defn desideratum->actions
  "Returns seq of actions, or nil if none."
  [fm id desideratum]
  (case desideratum
    :want-bdx (when (not (has-bdx? fm id))
                [`(:start-bdx-for ~id)])
    nil))

(defn do-action [fm action]
  (pmatch action
    (:start-bdx-for ~id)
      (start-bdx-for fm id)))

(defn post-actions-for-desiderata [fm]
  (with-state [fm fm]
    (doseq [node (g/nodes fm)
            desideratum (g/attr fm node :desiderata)]
      (update :actions into (desideratum->actions fm node desideratum)))))

;;; Running

(defn do-timestep [fm]
  (with-state [fm fm]
    (update :timestep inc)
    (assoc :actions #{}, :support-actions [])
    post-actions-for-desiderata
    (doseq [action (:actions fm)]
      (do-action action))
    post-support-for-desiderata
    -- (logdd :support (:support-actions fm))
    update-support-weights
    update-total-support
    ; TODO: rm unsupported elems
  ))

(defn bdxstr [fm bdxid]
  (let [[fromid toid] (->> bdxid
                           (g/incident-ports fm)
                           (sort-by second)
                           (map first))]
    (str bdxid \space fromid " -> " toid (g/attrstr fm bdxid))))

(defn pprint-bdx [fm]
  (cond
    :let [ids (all-bdx fm)]
    (empty? ids)
      (println "Bindings: None")
    (do
      (println "Bindings:")
      (doseq [s (->> ids (map #(bdxstr fm %)) sort)]
        (println \space s)))))

(defn supportstr [fm suppid]
  (let [[fromid toid] (->> suppid
                           (g/incident-ports fm)
                           (sort-by second)
                           (map first))]
    (str suppid "  " fromid " -> " toid "  " (g/weight fm suppid))))

(defn pprint-support [fm]
  (cond
    :let [ids (all-support-edges fm)]
    (empty? ids)
      (println "Support: None")
    (let [total (->> ids (map #(g/weight fm %)) (reduce +))]
      (println (str "Support:     (total " total ")"))
      (doseq [s (->> ids (map #(supportstr fm %)) sort)]
        (println \space s)))))

(defn pprint-model [fm]
  (println "Gattrs:" (g/gattrs fm))
  (g/pprint-nodes fm)
  ;TODO pprint-edges
  (pprint-bdx fm)
  (pprint-support fm))

(defn print-model-state [fm]
  #_(g/pprint fm)
  (pprint-model fm)
  fm)

(defn demo
  "Run this to see what farg.model1 does."
  [& logk]
  (with-logging logk
    (with-state [fm (start-model abc)]
      print-model-state
      (dotimes [t 10]
        do-timestep
        -- (println)
        print-model-state
      ))))
