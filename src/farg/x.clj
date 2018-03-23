(ns farg.x
  "Experimental code. Code here hopes to move quickly to another namespace.
  If not, it gets deleted."
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [com.rpl.specter :as S]
            [farg.gui :as gui]
            [farg.graphs :as g :refer [graph make-graph]]
            [farg.model :as m :refer [start-model]]
            [farg.no-reload :refer [gui-state]]
            [farg.util :as util :refer [dd find-first remove= with-rng-seed]]
            [farg.with-state :refer [with-state]]))

;(def letters-want
;  [(want 

(def abc (graph 
  (left-to-right-seq 'a 'b 'c)))

(defn bind-everything-to-everything [fm0]
  (with-state [fm fm0]
    (doseq [this-elem (g/elems fm0)
            that-elem (remove= this-elem (g/elems fm0))]
      (m/add-binding this-elem that-elem))))

(def fm0
 (with-state [fm (m/start-model abc)]
   (bind-everything-to-everything)
   ))
