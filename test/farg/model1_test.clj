(ns farg.model1-test
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer :all]
            [farg.graphs2 :as g :refer [graph make-graph]]
            [farg.model1 :as m :refer [set-letter-attrs update-nodes
              start-model all-bdx add-bdx start-bdx-from-for symbolically
              add-tag tags-of tagports-of tags-of-via-port-label has-tag?
              try-to-tag print-model-state bdx-from-to]]
            [farg.util :as util :refer [dd dde]]
            [farg.with-state :refer [with-state]]))

(defn =sets [xs ys]
  (= (set xs) (set ys)))

(deftest test-bdx
  (with-state [fm (start-model m/abc)]
    (is empty? (all-bdx fm))
    (start-bdx-from-for 'a)
    (is (=sets [[:bind 'a 'b] [:bind 'a 'c]]
               (->> (all-bdx fm) (map #(symbolically fm %)))))))

(deftest test-tags
  (with-state [fm (start-model m/abc)]
    (add-tag :testtag :from 'a :to 'b)
    (is (= [[:tag :testtag :from 'a :to 'b]]
           (map #(symbolically fm %) (tags-of fm 'a))))
    (is (= [[:tag :testtag :from 'a :to 'b]]
           (map #(symbolically fm %) (tags-of fm :testtag 'a))))
    (bind tagid (first (tags-of fm 'a)))
    (is (=sets [[tagid :from]] (tagports-of fm 'a)))
    (is (=sets [[tagid :to]] (tagports-of fm 'b)))
    (is (has-tag? fm :testtag :from 'a :to 'b))
    (is (not (has-tag? fm :testtag :from 'b :to 'a)))

    ;trying to tag them again should do nothing
    (add-tag :testtag :from 'a :to 'b)
    (is (= [tagid] (tags-of fm 'a)))))

(deftest test-tag-adjright
  (with-state [fm (start-model m/abc)]
    (try-to-tag 'a 'b)
    (is (has-tag? fm :adj-right :from 'a :to 'b))
    (try-to-tag 'b 'a)
    (is (not (has-tag? fm :adj-right :from 'b :to 'a)))

    ;trying to tag them again should do nothing
    (bind tagid (first (tags-of fm 'a)))
    (try-to-tag 'a 'b)
    (is (= [tagid] (tags-of fm 'a)))))

(deftest test-tag-makes-basis-edge
  (with-state [fm (start-model m/abc)]
    (add-bdx 'a 'b)
    (try-to-tag 'a 'b)
    (bind tagid (first (tags-of fm 'a)))
    (bind bdxids (bdx-from-to fm 'a 'b))
    (is (= 1 (count bdxids)))
    (bind bdxid (first bdxids))
    (is (g/has-edge? fm [tagid :basis-of] [bdxid :basis]))))

(deftest test-bdx-makes-tag
  (with-state [fm (start-model m/abc)]
    (try-to-tag 'a 'b)
    (add-bdx 'a 'b)
    (bind tagid (first (tags-of fm 'a)))
    (bind bdxids (bdx-from-to fm 'a 'b))
    (is (= 1 (count bdxids)))
    (bind bdxid (first bdxids))
    (is (g/has-edge? fm [tagid :basis-of] [bdxid :basis]))))
