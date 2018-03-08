(ns farg.layout-test
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer :all]
            [farg.graphs :as g :refer [graph]]
            [farg.layout :as ly :refer [lay-out]]; :refer [empty-layout update-layout]]
            [farg.util :as util :refer [dd dde]]
            [farg.with-state :refer [with-state]]))

(deftest test-basics
  (let [g (graph (left-to-right-seq 'a 'b 'c))]
    ;(g/pprint g)
    ;(lay-out g)
    ;(pprint (ly/implication-mates g 'a))
    (pprint (ly/implication-components g))
  ))
  ;(pprint (update-layout empty-layout
