(ns farg.graphs4-test
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer :all]
            [com.rpl.specter :as S]
            [farg.graphs4 :as g :refer [graph farg-spec]]
            [farg.util :as util :refer [dd =msets]]
            [farg.with-state :refer [with-state]]))

#_(pprint (macroexpand '(farg-spec
               (nodeclass :letter
                 (name-match? symbol?)
                 (attrs {:desiderata #{[:want :bdx]}})))))

(def abc-spec
  (farg-spec
    (nodeclass :letter
      (name-match? symbol?)
      (attrs {:desiderata #{[:want :bdx]}}))
    (edgeclass :bind
      (attrs {:self-support [:decaying 0.2]}))
    (stems {:bind 0})))

(deftest test-simple-spec
  (with-state [g (graph abc-spec 'a)]
    (is (= :letter (g/attr g 'a :class)))
    (is (= #{[:want :bdx]} (g/attr g 'a :desiderata)))

    (g/add-node 'b)
    (is (= :letter (g/attr g 'b :class)))
    (is (= #{[:want :bdx]} (g/attr g 'b :desiderata)))

    (setq bdxid (g/make-edge :bind ['a :bdx-from] ['b :bdx-to]))
    (is (= :bind001 bdxid))
    (is (= :bind (g/attr g bdxid :class)))
    (is (= [:decaying 0.2] (g/attr g bdxid :self-support)))))

(deftest test-nodeclass-inheritance
  (let [spec (farg-spec
               (nodeclass :node
                 (attrs {:a nil})
                 (port-labels :p))
               (nodeclass :number (extends :node)
                 (attrs {:n nil})))]
    (is (=msets [:a]    (g/nodeclass-attrs spec :node)))
    (is (=msets [:a :n] (g/nodeclass-attrs spec :number)))))

(deftest test-left-to-right-seq
  (let [g (graph (left-to-right-seq 'a 'b))]
    (is (= 'b (g/attr g 'a :adj-right)))
    (is (nil? (g/attr g 'a :adj-left)))
    (is (nil? (g/attr g 'b :adj-right)))
    (is (= 'a (g/attr g 'b :adj-left)))))

(deftest test-node-shorthand
  (let [g (graph abc-spec ['a :uncle :bob])]
    (is (= :bob (g/attr g 'a:bob :uncle)))
    (is (= :letter (g/attr g 'a:bob :class)))))

(def little-numbo-spec (farg-spec
  (nodeclass :number
    (name-match? number?)
    (port-labels :source :result :not-this))
  (nodeclass :brick
    (port-labels :source :result :not-this))  ;TODO inherit from :number
  (nodeclass :block
    (port-labels :source :result :not-this))
  (nodeclass :target
    (port-labels :source :result :not-this))
  (nodeclass :operator
    (name-match? #{:plus})
    (port-labels :operands :result))
  (portclass :source (extends :in))
  (portclass :result (extends :out))
  (portclass :operands (extends :in))
  (can-link :source :result)
  (can-link :result :operands)))

#_(pprint little-numbo-spec)

#_(pprint (macroexpand '(graph little-numbo-spec [4 -> :plus])))

(deftest test-edge-shorthand
  (let [g (graph little-numbo-spec 4 :plus [4 -> :plus])]
    (is (g/port-label-isa? g :source :in))
    (is (not (g/port-label-isa? g :source :out)))
    (is (g/has-edge? g [4 :result] [:plus :operands]))))

(deftest test-ctx
  (let [g (graph little-numbo-spec
            (ctx :problem
              [:target :n 15]
              [:brick :n 4]
              [:brick :n 5]
              [:brick :n 6]))]
    (is (g/has-node? g :problem))
    (is (g/has-edge? g [:problem :ctx-members] [:brick4 :ctx]))
    (is (g/has-edge? g [:problem :ctx-members] [:brick5 :ctx]))
    (is (g/has-edge? g [:problem :ctx-members] [:brick6 :ctx]))))

(deftest test-with-node-attrs
  (let [g (graph
            (with-node-attrs {:self-support [:permanent 1.0]}
              :a :b
              (with-node-attrs {:nested true}
                :c))
            :d)]
    (is (= {:self-support [:permanent 1.0]} (g/user-attrs g :a)))
    (is (= {:self-support [:permanent 1.0]} (g/user-attrs g :b)))
    (is (= {:self-support [:permanent 1.0], :nested true} (g/user-attrs g :c)))
    (is (= {} (g/user-attrs g :d)))))

(deftest test-v20-graph
  (let [g (graph little-numbo-spec
            (ctx :eqn
              [:number :n 11]
              [:number :n 5]
              [:number :n 6]
              :plus
              [5 -> :plus]
              [6 -> :plus]
              [:plus -> 11])
            (ctx :problem
              [:target :n 15]
              [:block :n 9]
              [:brick :n 4]
              [:brick :n 5]
              :plus
              [4 -> :plus]
              [5 -> :plus]
              [:plus -> 9]))]
    #_(g/pprint g)))
