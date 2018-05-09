(ns farg.graphs3-test
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer :all]
            [farg.graphs3 :as g :refer [graph farg-spec]]
            [farg.util :as util :refer [dd dde]]
            [farg.with-state :refer [with-state]]))

#_(pprint (macroexpand '(farg-spec
               (nodeclass :letter
                 (name-match? symbol?)
                 (attrs {:desiderata #{[:want :bdx]}})))))

(deftest test-simple-spec
  (let [spec (farg-spec
               (nodeclass :letter
                 (name-match? symbol?)
                 (attrs {:desiderata #{[:want :bdx]}}))
               (edgeclass :bind
                 (attrs {:self-support [:decaying 0.2]}))
               (stems {:bind 0}))]
    (with-state [g (graph spec 'a)]
      (is (= :letter (g/attr g 'a :class)))
      (is (= #{[:want :bdx]} (g/attr g 'a :desiderata)))

      (g/add-node 'b)
      (is (= :letter (g/attr g 'b :class)))
      (is (= #{[:want :bdx]} (g/attr g 'b :desiderata)))

      (setq bdxid (g/make-edge :bind ['a :bdx-from] ['b :bdx-to]))
      (is (= :bind001 bdxid))
      (is (= :bind (g/attr g bdxid :class)))
      (is (= [:decaying 0.2] (g/attr g bdxid :self-support)))
      )))

(deftest test-left-to-right-seq
  (let [g (graph (left-to-right-seq 'a 'b))]
    (g/pprint g)
    (is (= 'b (g/attr g 'a :adj-right)))
    (is (nil? (g/attr g 'a :adj-left)))
    (is (nil? (g/attr g 'b :adj-right)))
    (is (= 'a (g/attr g 'b :adj-left)))))
