(ns farg.graphs-test
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer :all]
            [farg.graphs :as g :refer [graph]]
            [farg.util :as util :refer [dd dde]]
            [farg.with-state :refer [with-state]]))

(defn mrounded [xx] (map util/mround xx))

(deftest test-left-to-right-seq
  (let [g (graph (left-to-right-seq 'a 'b))]
    (is (= 'b (g/attr g 'a :adj-right)))
    (is (nil? (g/attr g 'a :adj-left)))
    (is (nil? (g/attr g 'b :adj-right)))
    (is (= 'a (g/attr g 'b :adj-left)))
    (is (= [0.0 0.0] (g/attr g 'a :xy)))
    (is (= [1.0 0.0] (g/attr g 'b :xy)))
    ))

(deftest test-pt-on-ellipse
  (let [d45 (/ Math/PI 4.0), d90 (/ Math/PI 2.0)
        d135 (* 3 (/ Math/PI 4.0)), d180 Math/PI
        d-45 (/ Math/PI -4.0), d-90 (/ Math/PI -2.0)]
    (is (= [0.894 0.894] (mrounded (g/pt-on-ellipse [0 0] 2 1 d45))))
    (is (= [0.0 1.0] (mrounded (g/pt-on-ellipse [0 0] 2 1 d90))))
    (is (= [-0.894 0.894] (mrounded (g/pt-on-ellipse [0 0] 2 1 d135))))))

(deftest test-place-tag
  (with-state [g (graph (left-to-right-seq 'a 'b))]
    (g/add-tag :succ 'a 'b)
    ;Smoke test only
    -- (g/pprint g)
  ))
