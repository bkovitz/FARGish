(ns farg.timestep
  "Running one timestep."
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.core.reducers :as r]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [com.rpl.specter :as S]
            ;[farg.model :as model :refer [print-state make-model]]
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

;(defn start-bdx [state0]
;  (with-state [state state0]
;    (doseq [elem (model/all-needing-bdx state0)
;            other-elem (model/all-other-nodes state0 elem)]
;      (model/make-binding elem other-elem)
;      (model/make-binding other-elem elem))))
;
;(defn do-timestep [state]
;  (with-state [state state]
;    (update :timestep inc)
;    (start-bdx)))
;
;(defn run [initial-state]
;  (with-state [state initial-state]
;    (do-timestep)
;    -- (print-state state)
;    ))
;
;(def model1
;  (-> (make-model 'a 'b 'c)
;      (model/add-tag :succ 'a 'b)
;      (model/add-tag :succ 'b 'c)))
;
;#_(print-state model1)
;
;#_(run model1)
;
;#_(with-state [state model1]
;  (make-binding :a :c)
;  (make-binding :c :a)
;  -- (g/pprint (:ws state))
;  -- (println)
;  (make-binding :b :a)
;  (make-binding :a :b)
;  -- (g/pprint (:ws state))
;  )
