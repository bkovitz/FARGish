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
      (g/merge-default-attrs 'a {:class :letter, :letter 'a})
      (g/merge-default-attrs 'b {:class :letter, :letter 'b})
      (g/merge-default-attrs 'c {:class :letter, :letter 'c})
      ))

(defn start-model [fm]
  (with-state [fm fm]
    (assoc :timestep 0)
    ; TODO: give each node appropriate desiderata--or do this when making abc
    ; TODO: give each node appropriate support
    ))

(defn all-elems-other-than [fm id]
  (->> (g/elems fm) (remove= id)))

(defn add-bdx [fm from to]
  (g/add-edge fm [from :bdx-from] [to :bdx-to] {:class :bind}))

(defn start-bdx-for
  "Creates bindings to id from all other nodes, and from id to all other
  nodes."
  [fm0 id]
  (with-state [fm fm0]
    (doseq [that-elem (all-elems-other-than fm0 id)]
      (add-bdx id that-elem)
      (add-bdx that-elem id))))

(defn has-bdx?
  "Are any bindings, even if unofficial, linked to [id :bdx-from] or
  [id :bdx-to]?"
  [fm id]
  (or (seq (g/port->incident-edges fm [id :bdx-from]))
      (seq (g/port->incident-edges fm [id :bdx-to]))))

(defn seek-desiderata-for [fm id]
  (condp = (g/attr fm id :class)
    :letter (if (has-bdx? fm id)
              fm
              (start-bdx-for fm id))
    fm))

(defn seek-desiderata [fm]
  (reduce seek-desiderata-for fm (g/nodes fm)))

(defn do-timestep [fm]
  (-> (update fm :timestep inc)
      seek-desiderata 
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
