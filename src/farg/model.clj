(ns farg.model
  "Functions to make and modify FARG models."
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
