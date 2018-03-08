(ns farg.model-test
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer :all]
            #_[farg.model :as model :refer [blank-state make-model make-node
              add-tag print-state graph]]
            [farg.pgraph :as g]
            [farg.util :as util :refer [dd dde]]
            [farg.with-state :refer [with-state]]))

#_(deftest test-basics
  #_(with-state [state blank-state]
    (setq a (make-node 'a {:adj-right 'b}))
    (setq b (make-node 'b {:adj-left 'a}))
    -- (print-state state)
  )
  (let [g (graph (adj-seq 'a 'b))]
    (g/pprint g))
)
