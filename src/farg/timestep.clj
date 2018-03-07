(ns farg.timestep
  "Running one timestep."
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.core.reducers :as r]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [com.rpl.specter :as S]
            [farg.pgraph :as g :refer [pgraph]]
            [farg.util :as util :refer [dd dde]]
            [farg.with-state :refer [with-state]]))

;TODO
; Draw the graph on-screen
; Bind only to the most bindable-looking elems
;   "bindable-looking" = active, similar, maybe tagged/bound relevantly
; Let the trivial bindings decay away
; Positive feedback for good bindings
; Negative feedback for bad bindings
; Auto-tagging

;MAYBE
; :wants #{:bdx}
; distribute-weight: Like weighted-choice, but don't roll the dice yet

(def blank-state
  {:ws (pgraph), :timestep 0})

(defn print-state [state]
  (println "timestep:" (:timestep state))
  (g/pprint (:ws state)))

(defn make-node-attrs [nodename]
  (cond
    (= :bind nodename)
      {:class :bind, :a 0.2, :needs-bdx? false}
    (= :succ nodename)
      {:class :tag, :a 0.2, :needs-bdx? false}
    (symbol? nodename)
      {:class :letter, :letter nodename, :a 0.2, :needs-bdx? true}))

(defn make-node [{:keys [ws] :as state} nodename]
  (let [[ws id] (g/next-id ws nodename)
        ws (g/add-node ws id (make-node-attrs nodename))]
    [(assoc state :ws ws) id]))

(defn add-tag [state tag from to]
  (with-state [state state]
    (setq t (make-node tag))
    (update :ws g/add-edge [t :from] [from :tags])
    (update :ws g/add-edge [t :to] [to :tags])))

(defn tag? [{:keys [ws] :as state} elem]
  (= :tag (g/attr ws elem :class)))

(defn make-binding [state from to]
  (with-state [state state]
    (setq b (make-node :bind))
    (update :ws g/add-edge [b :from] [from :bdx])
    (update :ws g/add-edge [b :to] [to :bdx])))

(defn make-model [& letters]
  (with-state [state blank-state]
    (doseq [letter letters]
      (setq _ (make-node letter)))))

(defn all-needing-bdx [{:keys [ws] :as state}]
  (->> ws g/elems (filter #(g/attr ws % :needs-bdx?))))

(defn all-other-nodes [state elem]
  (->> state :ws g/nodes (remove #(or (tag? state elem)
                                      (= elem %)))))

(defn start-bdx [state0]
  (with-state [state state0]
    (doseq [elem (all-needing-bdx state0)
            other-elem (all-other-nodes state0 elem)]
      (make-binding elem other-elem)
      (make-binding other-elem elem))))

(defn do-timestep [state]
  (with-state [state state]
    (update :timestep inc)
    (start-bdx)))

(defn run [initial-state]
  (with-state [state initial-state]
    (do-timestep)
    -- (print-state state)
    ))

(def model1
  (-> (make-model 'a 'b 'c)
      (add-tag :succ 'a 'b)
      (add-tag :succ 'b 'c)))

#_(print-state model1)

(run model1)

#_(with-state [state model1]
  (make-binding :a :c)
  (make-binding :c :a)
  -- (g/pprint (:ws state))
  -- (println)
  (make-binding :b :a)
  (make-binding :a :b)
  -- (g/pprint (:ws state))
  )
