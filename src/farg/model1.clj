(ns farg.model1
  "Very simple FARG model: just make internal bindings in 'abc'."
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [com.rpl.specter :as S]
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
; seek-desiderata
; start-bdx
; support-desiderata
; oppose-inconsistencies
; post-support
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

(def abc
  (-> (graph (left-to-right-seq 'a 'b 'c))
      (g/merge-default-attrs 'a
        {:class :letter, :letter 'a, :self-support `(:permanent 1.0)})
      (g/merge-default-attrs 'b
        {:class :letter, :letter 'b, :self-support `(:permanent 1.0)})
      (g/merge-default-attrs 'c
        {:class :letter, :letter 'c, :self-support `(:permanent 1.0)})
      ))

(defn start-model [fm]
  (with-state [fm fm]
    (assoc :timestep 0
           :actions #{})
    ; TODO: give each node appropriate desiderata--or do this when making abc
    ; TODO: give each node appropriate support
    ))

(defn all-nodes-other-than [fm id]
  (->> (g/nodes fm) (remove= id)))

(defn add-bdx [fm from to]
  (g/add-edge fm [from :bdx-from] [to :bdx-to]
              {:class :bind
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

(defn seek-desiderata-for
  "Returns lazy seq of actions, or nil if none."
  [fm id]
  (condp = (g/attr fm id :class)
    :letter (when (not (has-bdx? fm id))
              [`(:start-bdx-for ~id)])
    nil))

(defn do-action [fm action]
  (pmatch action
    (:start-bdx-for ~id)
      (start-bdx-for fm id)))

(defn post-actions-for-desiderata [fm]
  (with-state [fm fm]
    (doseq [node (g/nodes fm)]
      (update :actions into (seek-desiderata-for fm node)))))

(defn update-total-support-for [fm elem]
  ;TODO Add in support via :support edges
  (pmatch (g/attr fm elem :self-support)
    (:permanent ~n)
      (g/set-attr fm elem :total-support n)
    (:decaying ~n)
      (-> fm
          (g/set-attr elem :total-support n)
          (g/set-attr elem :self-support `(:decaying ~(* 0.9 n))))))

(defn update-total-support [fm]
  (with-state [fm fm]
    (doseq [elem (g/elems fm)]
      (update-total-support-for elem))))

(defn do-timestep [fm]
  (with-state [fm fm]
    (update :timestep inc)
    (assoc :actions #{})
    post-actions-for-desiderata
    (doseq [action (:actions fm)]
      (do-action action))
    update-total-support
    ; TODO
  ))

(defn print-model-state [fm]
  (g/pprint fm)
  fm)

(defn demo
  "Run this to see what farg.model1 does."
  []
  (with-state [fm (start-model abc)]
    print-model-state
    (dotimes [t 1]
      do-timestep
      -- (println)
      print-model-state
    )))
