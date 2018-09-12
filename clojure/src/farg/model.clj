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

;(def start-gattrs
;  {:timestep 0
;   :support-deltas

(defn start-model [fm]
  (g/set-gattr fm :timestep 0))

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

(defn run
 ([]
  (run simple-model))
 ([g]
  (gui/run (start-model g))))

;;; The new way ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;NEXT
; make-node  Set up initial activation and support
; normalize-support
; :self-support

(def default-initial-activation
  {:a 1.0, :min-activation 0.0})

(defn set-initial-activation [fm elem]
  (g/merge-default-attrs fm elem default-initial-activation))
  ;(g/set-attrs fm elem (merge default-initial-activation (g/attrs fm elem))))

(def default-initial-support
  {:total-support 0.2, :self-support '(decaying 0.2)})

(defn set-initial-support [fm elem]
  (g/merge-default-attrs fm elem default-initial-support))
  ;(g/set-attrs fm elem (merge default-initial-support (g/attrs fm elem))))

(defn make-node
 ([fm nodename]
  (make-node fm nodename {}))
 ([fm nodename attrs]
  (with-state [fm fm]
    (setq id (g/make-node nodename attrs))
    (set-initial-activation id)
    (set-initial-support id)
    (return [fm id]))))

(defn add-binding [fm from to]
  (with-state [fm fm]
    (setq b (make-node :bind))
    (g/add-edge [b :from] [from :bdx])
    (g/add-edge [b :to] [to :bdx])))

(defn post-support-delta
  "Queues up a support delta in fm."
  [fm agent recipient amount]
  (if (< amount 0.001)
    fm
    (update fm :support-deltas conj [agent recipient amount])))

(defn total-support-to
  "Returns total amount of support given to elem."
  [fm elem]
  (if-let [total-support (g/attr fm elem :total-support)]
    total-support
    0.0))

(defn post-support-for-support
  "Positive feedback: give support to elems that already have support."
  [fm agent recipient]
  (post-support-delta fm agent recipient
                      (* 0.1 (total-support-to fm recipient))))

(defn post-support-deltas
  [fm0]
  (with-state [fm fm0]
    (doseq [node (g/nodes fm0)
            neighbor (g/neighbors-of fm0 node)]
      (post-support-for-support node neighbor)
    )
  ))

(defn do-support-delta
  [fm [agent recipient amount]]
  (g/update-edge-attr fm [agent :support-to] [recipient :support-from] :weight
                         (fnil + 0.0) amount))

;NEXT
;(defn step-activation

(defn- refresh-total-support-for
  [fm elem]
  (let [total-support (->> (g/port->incident-edges fm [elem :support-from])
                           (map #(g/attr fm % :weight))
                           (reduce +))]
    (g/set-attr fm elem :total-support total-support)))

(defn refresh-total-support [fm]
  (with-state [fm fm]
    (doseq [elem (g/elems fm)]
      (refresh-total-support-for elem))))

(def default-attrs-for-original-nodes
  {:total-support 1.0, :self-support '(permanent 1.0),
   :a 1.0, :min-activation 0.1})

(defn start-model [fm]
  (with-state [fm fm]
    (assoc :timestep 0)
    (doseq [node (g/nodes fm)]
      (g/merge-default-attrs node default-attrs-for-original-nodes))
    ))

