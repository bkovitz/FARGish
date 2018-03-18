(ns farg.model
  "Functions to make and modify FARG models."
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [com.rpl.specter :as S]
            [farg.gui :as gui]
            [farg.graphs :as g :refer [graph make-graph]]
            [farg.no-reload :refer [gui-state]]
            [farg.util :as util :refer [dd find-first with-rng-seed]]
            [farg.with-state :refer [with-state]]))

;(def blank-state
;  {:ws (pgraph), :timestep 0})
;
;(defn print-state [state]
;  (println "timestep:" (:timestep state))
;  (g/pprint (:ws state)))
;
;(defn make-model [& letters]
;  (with-state [state blank-state]
;    (doseq [letter letters]
;      (setq _ (make-node letter)))))
;
;
;(defn all-needing-bdx [{:keys [ws] :as state}]
;  (->> ws g/elems (filter #(g/attr ws % :needs-bdx?))))
;
;(defn all-other-nodes [state elem]
;  (->> state :ws g/nodes (remove #(or (tag? state elem)
;                                      (= elem %)))))

(defn fm [] (:fm @gui-state))

(defn needs-bdx? [g elem]
  (g/attr g elem :needs-bdx?))

(defn all-needing-bdx [g]
  (filter #(needs-bdx? g %) (g/elems g)))

(defn all-other-primary-nodes [g elem]
  (filter #(and (g/primary-node? g %)
                (not= elem %))
          (g/nodes g)))

(defn start-bdx [g0]
  (with-state [g g0]
    (doseq [elem (all-needing-bdx g0)]
      (doseq [other-elem (all-other-primary-nodes g0 elem)]
        (g/add-binding elem other-elem))
      (g/set-attr elem :needs-bdx? false)
      )))

(defn do-timestep [g]
  (with-state [g g]
    (g/set-gattr :timestep (inc (g/gattr g :timestep)))
    (start-bdx)))

(defn start-model [g]
  (g/set-gattr g :timestep 0))

(def simple-model
  (with-state [g (graph (left-to-right-seq 'a 'b 'c))]
    (doseq [node (g/nodes g)]
      (g/set-attr node :needs-bdx? true))))

(defn step! []
  (swap! gui-state update :fm do-timestep)
  (gui/update!))

(defn one! []
  (swap! gui-state update :fm (fn [g]
    (g/add-binding g 'a 'b)))
  (gui/update!))

;NEXT (run) and (step!) should run the model and show what's happening in the
; GUI.
(defn run
 ([]
  (run simple-model))
 ([g]
  (gui/run (start-model g))))
