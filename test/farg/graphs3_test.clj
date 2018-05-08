(ns farg.graphs3-test
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer :all]
            [farg.graphs3 :as g :refer [graph farg-spec]]
            [farg.util :as util :refer [dd dde]]
            [farg.with-state :refer [with-state]]))

(pprint (macroexpand '(farg-spec
               (nodeclass :letter
                 (name-match? symbol?)
                 (attrs {:desiderata #{[:want :bdx]}})))))

(deftest test-simple-spec
  (let [spec (farg-spec
               (nodeclass :letter
                 (name-match? symbol?)
                 (attrs {:desiderata #{[:want :bdx]}})))
        g (graph spec 'a)]
    (pprint (g/attrs g 'a))
    (is (= :letter (g/attr g 'a :class)))
    (is (= #{[:want :bdx]} (g/attr g 'a :desiderata)))))

#_(deftest test-left-to-right-seq
  (let [g (graph (left-to-right-seq 'a 'b))]
    (is (= 'b (g/attr g 'a :adj-right)))
    (is (nil? (g/attr g 'a :adj-left)))
    (is (nil? (g/attr g 'b :adj-right)))
    (is (= 'a (g/attr g 'b :adj-left)))))
