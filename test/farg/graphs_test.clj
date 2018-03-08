(ns farg.graphs-test
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer :all]
            [farg.graphs :as g :refer [graph]]
            [farg.util :as util :refer [dd dde]]
            [farg.with-state :refer [with-state]]))

(deftest test-left-to-right-seq
  (let [g (graph (left-to-right-seq 'a 'b))]
    (is (= 'b (g/attr g 'a :adj-right)))
    (is (nil? (g/attr g 'a :adj-left)))
    (is (nil? (g/attr g 'b :adj-right)))
    (is (= 'a (g/attr g 'b :adj-left)))
    ))
