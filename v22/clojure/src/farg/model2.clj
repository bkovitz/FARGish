(ns farg.model2
  "Very simple FARG model: just make internal bindings in 'abc'."
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.core.strint :refer [<<]]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [com.rpl.specter :as S]
            [farg.logging :as log :refer [with-logging log logdd logdo]]
            [farg.gui :as gui]
            [farg.graphs4 :as g :refer [graph make-graph farg-spec]]
            [farg.no-reload :refer [gui-state]]
            [farg.pmatch :refer [pmatch]]
            [farg.util :as util :refer [dd remove= find-first with-rng-seed
              mapvals =sets]]
            [farg.with-state :refer [with-state]]))

;;; Utility functions

(defn close-to-zero? [x]
  (< (Math/abs x) 0.001))

(defn +abs
 ([]
  0.0)
 ([x1]
  (Math/abs x1))
 ([x1 x2]
  (+ (Math/abs x1) (Math/abs x2))))

(defn max2 [coll]
  (loop [coll coll, x1 nil, x2 nil]
    (cond
      (empty? coll)
        (filter some? [x1 x2])
      :let [x (first coll)]
      (nil? x1)
        (recur (rest coll) x nil)
      (> x x1)
        (recur (rest coll) x x1)
      (> x x2)
        (recur (rest coll) x1 x)
      (recur (rest coll) x1 x2))))

(defn all-nodes-other-than [fm id]
  (->> (g/nodes fm) (remove= id)))

(defn class-of [fm id]
  (g/attr fm id :class))

(defn tagclass-of [fm id]
  (g/attr fm id :tagclass))

(defn unimplemented [& ignored]
  (throw (IllegalArgumentException. "Unimplemented function.")))

;;; Saving data

(def data (atom {})) ;{series-name [{column-name value}]}

(defn obs
  "Adds one observation to @data. series is the name of the series.
  datum is a map of the form {column-name value}."
  [series-name datum]
  (swap! data update series-name (fnil conj []) datum))

(defn clear-data! []
  (reset! data {}))

(defn rm-unicode
  "R doesn't seem to be able to handle Unicode characters in data."
  [s]
  (clojure.string/replace s #"↦" "->"))

(defn write-data-series [series-name]
  (let [series (get @data series-name)]
    (util/with-*out* (clojure.java.io/writer (str (name series-name) ".csv"))
      (let [column-names (->> series first keys (map str) sort)]
        (println (clojure.string/join \, column-names)))
      (doseq [row series]
        (println (rm-unicode (clojure.string/join \,
          (->> row (sort-by #(-> % first str)) (map second)))))))))

(defn write-data []
  (doseq [series-name (keys @data)]
    (write-data-series series-name)))

;;; Map objects

(def empty-desideratum ^{:type ::desideratum}
  {:type ::desideratum
   ::desideratum-type nil
   ::ospec-type ::desideratum
   ::ospecs #{}})

(defn need [ospec & ospecs]
  (assoc empty-desideratum ::desideratum-type ::need
                           ::ospecs (apply hash-set ospec ospecs)))

(defn want [ospec & ospecs]
  (assoc empty-desideratum ::desideratum-type ::want
                           ::ospecs (apply hash-set ospec ospecs)))

(defn oppose [ospec & ospecs]
  (assoc empty-desideratum ::desideratum-type ::oppose
                           ::ospecs (apply hash-set ospec ospecs)))

(defmethod print-method ::desideratum
  [d ^java.io.Writer w]
  (.write w
    (<< "(~(name (::desideratum-type d)) "
        "~(clojure.string/join \\space (::ospecs d)))")))

(def empty-ospec  ;prototype for an "object specification": objects
  {:type ::ospec  ;  needed by a desideratum
   ::ospec-type nil
   ::preference? false})

(defn preference? [ospec]
  (get ospec ::preference?))

(defn bdx-to-ctx [from-ctx to-ctx]
  (with-meta
    (assoc empty-ospec ::ospec-type ::bdx-to-ctx
                       ::from-ctx from-ctx
                       ::to-ctx to-ctx)
    {:type ::bdx-to-ctx}))

(defn bdx-from-ctx [from-ctx to-ctx]
  (with-meta
    (assoc empty-ospec ::ospec-type ::bdx-from-ctx
                       ::from-ctx from-ctx
                       ::to-ctx to-ctx)
    {:type ::bdx-from-ctx}))

(defn prefer [ospec & ospecs]
  (with-meta 
    (assoc empty-ospec ::ospec-type ::preference
                       ::preference? true
                       ::ospecs (apply hash-set ospec ospecs))
    {:type ::preference}))

; This is how it should look:
;  (need :bdx (prefer :has-basis))

(defmethod print-method ::preference
  [o ^java.io.Writer w]
  (.write w
    (<< "(prefer ~(clojure.string/join \\space (::ospecs o)))")))

(defmethod print-method ::bdx-to-ctx
  [{::keys [from-ctx to-ctx]} ^java.io.Writer w]
  (.write w
    (<< "(bdx-to-ctx ~{from-ctx} ~{to-ctx})")))

(defmethod print-method ::bdx-from-ctx
  [{::keys [from-ctx to-ctx]} ^java.io.Writer w]
  (.write w
    (<< "(bdx-from-ctx ~{from-ctx} ~{to-ctx})")))

;;; Global parameters (these belong in the FARG model itself)

(def default-globals
  {:need-delta 0.1
   :preference-delta 0.1
   :antipathy-per-timestep -0.5
   :positive-feedback-rate 0.1
   :minimum-total-support 0.02
   :minimum-self-support 0.02
   :normalization-expt 1.5
    ; Applied when normalizing a set of values whose sum exceeds some limit.
    ; Must be > 1.0 for higher values to drive down lower values.
   :normalize-total-supports? true
   :cnormalize-method :by-exponent
   :request-orientation? true
   :init nil  ;function to call at start of run
   :scheduled nil  ;map {timestep func} of funcs to call at start of
                   ;specified timesteps
   :quiet? false
  })

(defn need-delta [fm] (get fm :need-delta))
(defn preference-delta [fm] (get fm :preference-delta))
(defn antipathy-per-timestep [fm] (get fm :antipathy-per-timestep))
(defn positive-feedback-rate [fm] (get fm :positive-feedback-rate))
(defn minimum-total-support [fm] (get fm :minimum-total-support))
(defn minimum-self-support [fm] (get fm :minimum-self-support))
(defn normalization-expt [fm] (get fm :normalization-expt))

(defn get-timestep [fm] (get fm :timestep))

#_(defn raise-to-norm-expt [n]
  (cond
    (> n 0.0)
      (Math/pow n normalization-expt)
    (< n 0.0)
      (- (Math/pow (- n) normalization-expt))
    0.0))

(defn diag-sigmoid
  "p is exponent. p=2.0 for the classic S shape winding around y=x. Values
  close to 0 are made closer to 0; values close to 1 are made closer to 1.
  A smaller p reduces the amount by which x is modified."
  [p x]
  (assert (<= 0.0 x 1.0))
  (/ (* 1.0 (Math/pow x p))
     (+ (* 1.0 (Math/pow x p))
        (Math/pow (- 1 x) p))))

(defn oddf [f]
  (fn [x]
    (cond
      (pos? x)
        (f x)
      (neg? x)
        (- (f (- x)))
      0.0)))

(defn unit-scaled
  "Scales the elements of coll, assumed to be numbers, so the largest
  absolute value is 1.0."
  [coll]
  (cond
    (empty? coll)
      coll
    :let [scaling-factor (/ (->> (map #(Math/abs %) coll) (apply max)))]
    (map #(* scaling-factor %) coll)))

(defn cnormalize-vals-
  "Competitively normalize the vals of m. cnorm-f must be a function that
  takes an argument in [-1.0, +1.0], where +1.0 represents the greatest
  val to be normalized."
  [sum-f cnorm-f target-sum m]
  (cond
    (empty? m)
      m
    :let [vs (vals m)
          sum (reduce sum-f vs)]
    (or (<= sum target-sum) (zero? sum))
      m
    :let [new-vs (map cnorm-f (unit-scaled vs))
          sum (reduce sum-f new-vs)
          scaling-factor (/ target-sum sum)]
    (zipmap (keys m)
            (map #(* scaling-factor %) new-vs))))

(defn pow-pos
  "Returns x^y if x is non-negative, -(-x)^y if if is negative. This lets you
  exponentiate negative numbers so that they fall as x decreases the same
  way that exponentiated positive numbers grow as x increases."
  [x y]
  (cond
    (pos? x)
      (Math/pow x y)
    (neg? x)
      (- (Math/pow (- x) y))
    0.0))

(defn expt-scale-down-vals
 ([expt target-sum m]
  (expt-scale-down-vals expt target-sum +abs m))
 ([expt target-sum sumf m]
  (cond
    (empty? m)
      m
    :let [vs (vals m)
          sum (reduce sumf vs)]
    (or (<= sum target-sum) (zero? sum))
      m
    :let [new-vs (map #(pow-pos % expt) vs)
          sum (reduce sumf new-vs)
          scaling-factor (/ target-sum sum)]
    (zipmap (keys m)
            (map #(* scaling-factor %) new-vs)))))

(defn cnormalize-vals [fm target-sum m]
  (let [p (normalization-expt fm)]
    (case (:cnormalize-method fm)
      :by-exponent
        (expt-scale-down-vals p target-sum m)
      :by-sigmoid
        (cnormalize-vals- +abs (oddf (partial diag-sigmoid p)) target-sum m))))

;;; Utility functions for support

(defn support? [fm id]
  (= :support (class-of fm id)))

(defn all-support-edges [fm]
  (filter #(support? fm %) (g/edges fm)))

(defn support-edges-and-weights
  "Returns seq of [edgeid weight], one element for each :support edge in fm."
  [fm]
  (for [edgeid (all-support-edges fm)]
    [edgeid (g/weight fm edgeid)]))

(defn support-edges-and-attempted-weights
  "Returns seq of [edgeid attempted-weight], one element for each :support
  edge in fm."
  [fm]
  (for [edgeid (all-support-edges fm)]
    [edgeid (g/attr fm edgeid :attempted-weight)]))

#_(defn support-edges-and-abs-weights
  "Returns seq of [edgeid (Math/abs weight)], one element for each :support
  edge in fm."
  [fm]
  (for [edgeid (all-support-edges fm)]
    [edgeid (Math/abs (g/weight fm edgeid))]))

(defn elems-that-can-give-support [fm]
  (filter #(contains? (g/attrs fm %) :total-support) (g/elems fm))
  #_(filter #(not (support? fm %)) (g/elems fm)))

(def elems-that-can-receive-support elems-that-can-give-support)

(defn find-support-edge
  "Returns nil if :support edge does not exist."
  [fm fromid toid]
  (g/find-edgeid fm [fromid :support-to] [toid :support-from]))

(defn remove-support-edge [fm fromid toid]
  (g/remove-edge fm [fromid :support-to] [toid :support-from]))

(defn add-support-edge [fm fromid toid weight]
  (g/add-edge fm nil [fromid :support-to] [toid :support-from]
                 {:class :support
                  :prev-weight 0.0
                  :weight weight
                  :attempted-weight weight}))

(defn find|make-support-edge
  "Returns [fm edgeid] where edgeid is specified :support edge. Creates the
  edge with :prev-weight, :attempted-weight, and :weight all 0.0 if it doesn't
  exist."
  [fm fromid toid]
  (cond
    :let [edgeid (find-support-edge fm fromid toid)]
    (some? edgeid)
      [fm edgeid]
    :let [fm (add-support-edge fm fromid toid 0.0)
          edgeid (find-support-edge fm fromid toid) ;TODO optimize
          _ (assert (some? edgeid))]
    [fm edgeid]))

(defn outgoing-support-edges [fm fromid]
  (g/port->incident-edges fm [fromid :support-to]))

(defn incoming-support-edges [fm toid]
  (g/port->incident-edges fm [toid :support-from]))

(defn incoming-support-weights [fm toid]
  (map #(g/weight fm %) (incoming-support-edges fm toid)))

(defn supportees-of
  "Elements (both nodes and edges) to which there is a support edge from
  fromid. Note that a supportee might be getting negative support."
  [fm fromid]
  (->> (outgoing-support-edges fm fromid)
       (map #(g/other-id fm fromid %))))

(defn attempted-weight-of [fm suppid]
  (or (g/attr fm suppid :attempted-weight) 0.0))

(defn self-support-for [fm elem]
  (pmatch (g/attr fm elem :self-support)
    [:permanent ~n]
      n
    [:decaying ~n]
      n
    nil
      0.0))

(defn total-self-support [fm]
  (->> fm
       elems-that-can-give-support
       (map #(self-support-for fm %))
       (reduce +)))

(defn self-support-available-from [fm fromid]
  (cond
    :let [prev (g/attr fm fromid :prev-self-support)]
    (some? prev)
      prev
    (self-support-for fm fromid)))

(defn total-support-available [fm]
  (if-let [sl (:support-limit fm)]
    sl
    (->> fm
         elems-that-can-give-support
         (map #(self-support-available-from fm %))
         (reduce +))))

(defn total-support-for [fm id]
  (cond
    :let [s (g/attr fm id :total-support)]
    (nil? s) 0.0
    s))

(defn prev-total-support-for [fm id]
  (cond
    :let [s (g/attr fm id :prev-total-support)]
    (some? s) s
    :let [s (self-support-for fm id)]
    (some? s) s
    0.0))

;TODO UT
(defn support-map
  "Returns map {fromid {toid weight}} of all existing support in fm."
  [fm]
  (with-state [m {}]
    (doseq [edgeid (all-support-edges fm)]
      (bind emap (g/edge-as-map fm edgeid))
      (assoc-in [(:support-to emap) (:support-from emap)] (:weight emap)))))

(defn total-of-all-support-weights [fm]
  (->> (all-support-edges fm)
       (map #(g/weight fm %))
       (reduce +abs)))

(defn total-of-all-pos-support-weights [fm]
  (->> (all-support-edges fm)
       (map #(g/weight fm %))
       (filter pos?)
       (reduce +)))

(defn permanent-support-for [fm id]
  (pmatch (g/attr fm id :self-support)
    [:permanent ~n]
      n
    ~any
      0.0))

(defn total-permanent-support [fm]
  (->> (elems-that-can-receive-support fm)
       (map #(permanent-support-for fm %))
       (reduce +)))

(defn total-total-support [fm]
  (->> (elems-that-can-receive-support fm)
       (map #(total-support-for fm %))
       (filter pos?)
       (reduce +)))

;;; Initialization

(def letter-attrs
  {:class :letter
   :self-support [:permanent 1.0]
   ;:desiderata #{[:need :bdx [:prefer :bdx-from :adj-bdx]]}
   :desiderata #{(need :bdx (prefer :bdx-from :adj-bdx))} ;the NEW WAY
   :total-support nil
   ;:desiderata #{:want-bdx :want-adj-bdx}
   ;:desiderata #{:want-bdx-from}
   ;:desiderata #{:want-bdx}
   })

(defn update-nodes [fm idmatchf? updatef]
  (with-state [fm fm]
    (doseq [id (g/nodes fm)]
      (when (idmatchf? id)
        (updatef id)))))

(defn set-letter-attrs [fm id attrs]
  (g/merge-default-attrs fm id attrs {:letter id}))

(def abc
  (-> (graph (left-to-right-seq 'a 'b 'c))
      (update-nodes symbol? #(set-letter-attrs %1 %2 letter-attrs))))

(defn init [fm]
  (if-let [init-fn (:init fm)]
    (init-fn fm)
    fm))

(defmethod print-method ::farg-model [fm ^java.io.Writer w]
  (.write w (<< "#farg-model[timestep ~(:timestep fm), "
                "nodes ~(count (g/nodes fm)), "
                "edges ~(count (g/edges fm))]")))

(defn start-model
  "override-params is a map that gets merged onto default-globals."
 ([fm]
  (start-model fm {}))
 ([fm override-params]
  (with-state [fm (with-meta
                    (merge fm default-globals override-params)
                    {:type ::farg-model})]
    (update :stems assoc :bind 0 :support 0 :tag 0)
    (assoc :timestep 0
           :actions #{}
           :queued-actions #{}
           :support-deltas {})  ; map {fromid {toid amt}}
    init)))

;;; Bindings

(defn bdx? [fm id]
  (= :bind (class-of fm id)))

(defn all-bdx [fm]
  (->> fm g/edges (filter #(bdx? fm %))))

(declare symbolically)
(declare add-basis-from-bdx)

(defn all-bdx-symbolically [fm]
  (->> (all-bdx fm) (map #(symbolically fm %))))

(defn add-bdx [fm from to]
  (with-state [fm fm]
    (g/add-edge :bind [from :bdx-from] [to :bdx-to]
                {:class :bind
;                 :desiderata #{[:need :mates-to-exist]
;                               [:want :basis]
;                               [:oppose :other-bdx-from-same-mate]
;                               [:oppose :bindbacks]}
                 :desiderata #{(need :mates-to-exist) ;the NEW WAY
                               (want :basis)
                               (oppose :other-bdx-from-same-mate)
                               (oppose :bindbacks)}
  ;               :desiderata #{:want-mates-to-exist
  ;                             :oppose-other-bdx-from-same-mate
  ;                             :oppose-bindbacks}
                 :self-support [:decaying 0.2]
                 :total-support nil})
    (bind bdxid (g/find-edgeid fm [from :bdx-from] [to :bdx-to]))
    (add-basis-from-bdx bdxid)))

;TODO Rm this hack. It should be implemented by (extends) in the spec.
(def hack-h
  (-> (make-hierarchy)
      (derive :brick :number)
      (derive :block :number)
      (derive :target :number)))

(defn isa-number? [fm id]
  (isa? hack-h (class-of fm id) :number))

(defn same-number? [fm id1 id2]
  (cond
    (not (and (isa-number? fm id1)
              (isa-number? fm id2)))
      false
    :let [n1 (g/attr fm id1 :n)]
    (nil? n1)
      false
    :let [n2 (g/attr fm id2 :n)]
    (nil? n2)
      false
    (= n1 n2)))

(defn different-number? [fm id1 id2]
  (cond
    (not (and (isa-number? fm id1)
              (isa-number? fm id2)))
      false
    :let [n1 (g/attr fm id1 :n)]
    (nil? n1)
      false
    :let [n2 (g/attr fm id2 :n)]
    (nil? n2)
      false
    (not= n1 n2)))

;TODO Hard-coded; data for this decision belongs in spec.
;TODO Eventually, the criteria for "reasonable" need to be dynamic and
;subject to pressures.
(defn could-reasonably-bind? [fm fromid toid]
  (let [from-class (class-of fm fromid)
        to-class (class-of fm toid)]
    (or (isa? hack-h from-class to-class)
        (isa? hack-h to-class from-class)))
  #_(case (class-of fm fromid)
    :letter
      (= :letter (class-of fm toid))
    :bdx
      (= :bdx (class-of fm toid))
    :tag
      (= :tag (class-of fm toid))))
  
(defn incident-bdx-from [fm id]
  (g/port->incident-edges fm [id :bdx-from]))

(defn incident-bdx-to [fm id]
  (g/port->incident-edges fm [id :bdx-to]))

(defn incident-bdx [fm id]
  (concat (incident-bdx-from fm id)
          (incident-bdx-to fm id)))

(defn has-bdx?
  "Are any bindings, even if unofficial, linked to [id :bdx-from] or
  [id :bdx-to]?"
  [fm id]
  (or (seq (g/port->incident-edges fm [id :bdx-from]))
      (seq (g/port->incident-edges fm [id :bdx-to]))))

(defn bdx-from-to [fm fromid toid]
  (clojure.set/intersection (set (incident-bdx-from fm fromid))
                            (set (incident-bdx-to fm toid))))

(defn has-bdx-from?
  "Are any bindings, even if unofficial, linked to [id :bdx-from]?"
  [fm id]
  (seq (incident-bdx-from fm id)))

(defn bound-from
  "Returns id of element that bdxid is bound from."
  [fm bdxid]
  (->> (g/edge->incident-ports fm bdxid)
       (filter #(= :bdx-from (second %)))
       ffirst))

(defn bound-to
  "Returns id of element that bdxid is bound to."
  [fm bdxid]
  (->> (g/edge->incident-ports fm bdxid)
       (filter #(= :bdx-to (second %)))
       ffirst))

(defn bound-from? [fm fromid bdxid]
  (= fromid (bound-from fm bdxid)))

(defn bound-to? [fm toid bdxid]
  (= toid (bound-to fm bdxid)))

(defn bound-from-or-to? [fm id bdxid]
  (or (bound-from? fm id bdxid)
      (bound-to? fm id bdxid)))

(defn members-of [fm ctxid]
  (g/port->neighbors fm [ctxid :ctx-members]))

(defn member-of? [fm id ctxid]
  (some #{id} (members-of fm ctxid)))

(defn bound-to-elem-in? [fm bdxid ctxid]
  (member-of? fm (bound-to fm bdxid) ctxid))

(defn bound-from-elem-in? [fm bdxid ctxid]
  (member-of? fm (bound-from fm bdxid) ctxid))

(defn symbolically [fm elem]
  (case (class-of fm elem)
    :bind
      [:bind (bound-from fm elem) (bound-to fm elem)]
    :tag
      [:tag (tagclass-of fm elem) :from (g/port->neighbor fm [elem :from])
                                  :to (g/port->neighbor fm [elem :to])]
    :letter
      [:letter (g/attr fm elem :letter)]))

(defn edge-to-adjacent? [fm id edgeid]
  (let [mate (g/other-id fm id edgeid)]
    (or (= mate (g/attr fm id :adj-right))
        (= mate (g/attr fm id :adj-left)))))

(defn bindbacks
  "Returns seq of ids of bindings that are the reverse of bdxid: they bind
  from what bdxid binds to, and to what bdxid binds from."
  [fm bdxid]
  (let [fromid (bound-from fm bdxid)
        toid (bound-to fm bdxid)]
    (for [edgeid (incident-bdx-to fm fromid)
          :when (= toid (bound-from fm edgeid))]
      edgeid)))

;TODO UT
(defn bad-nbr-bdx [fm elemid]
  "Returns seq of bdxids that bind incident nodes to nodes that don't link
  to the image of elemid at the same port-label."
  (for [my-bdxid (incident-bdx-from fm elemid)
        :let [elemid' (bound-to fm my-bdxid)]
        port-label [:source :result :operands]  ;HACK
        [nbrid nbrpl] (g/port->neighboring-ports fm [elemid port-label])
        nbr-bdxid (incident-bdx-from fm nbrid)
        :let [nbr' (bound-to fm nbr-bdxid)]
        :when (and (g/elem->neighbor? fm elemid' nbr')
                   (not (g/has-edge? fm [elemid' port-label] [nbr' nbrpl])))]
    nbr-bdxid))

;TODO UT
#_(defn bdx-to-different-number [fm elemid]
  "Returns seq of bdxids that go to a different number than elemid, or nil
  if elemid is not itself a number."
  (when (isa-number? fm elemid)
    (let [n (g/attr fm elemid :n)]
      (for [bdxid (incident-bdx-from fm elemid)
            :let [elemid' (bound-to fm bdxid)]
            :when (and (isa-number? fm elemid')
                       (not= n (g/attr fm elemid' :n)))]
        bdxid))))

(declare has-tag?)

(defn bdx-to-different-number [fm elemid]
  "Returns seq of bdxids that go to a different number than elemid, or nil
  if elemid is not itself a number."
  (when (isa-number? fm elemid)
    (let [n (g/attr fm elemid :n)]
      ;TODO Just see if the bdxid itself is tagged :different-number
      (for [bdxid (incident-bdx-from fm elemid)
            :when (has-tag? fm :different-number
                            :from (bound-from fm bdxid)
                            :to (bound-to fm bdxid))]
        bdxid))))

(defn start-bdx-for
  "Creates bindings to id from all other nodes, and from id to all other
  nodes."
  [fm0 id]
  (with-state [fm fm0]
    (doseq [that-elem (all-nodes-other-than fm0 id)]
      (when (could-reasonably-bind? fm0 id that-elem)
        (add-bdx id that-elem))
      (when (could-reasonably-bind? fm0 that-elem id)
        (add-bdx that-elem id)))))

(defn start-bdx-from-for
  "Creates bindings from id to all other nodes."
  [fm0 fromid]
  (with-state [fm fm0]
    (doseq [toid (->> (all-nodes-other-than fm0 fromid)
                      (filter #(could-reasonably-bind? fm0 fromid %)))]
      (add-bdx fromid toid))))

(defn start-bdx-into-ctx-for
  [fm0 fromid to-ctx]
  (with-state [fm fm0]
    (doseq [toid (->> (members-of fm0 to-ctx)
                      (filter #(could-reasonably-bind? fm0 fromid %)))]
      (add-bdx fromid toid))))

(defn start-bdx-from-ctx-for
  [fm0 toid from-ctx]
  (with-state [fm fm0]
    (doseq [fromid (->> (members-of fm0 from-ctx)
                        (filter #(could-reasonably-bind? fm0 % toid)))]
      (add-bdx fromid toid))))

;; ospec multimethods

(declare ospec-find-objects)

(defmulti ospec-find-objects-
  (fn [fm ospec id]
    (or (and (keyword? ospec) ::keyword)
        (::ospec-type ospec)
        ospec)))

(defmethod ospec-find-objects- ::bdx-to-ctx
  [fm {::keys [from-ctx to-ctx]} id]
  (for [bdxid (incident-bdx-from fm id)
        :when (bound-to-elem-in? fm bdxid to-ctx)]
    bdxid))

(defmethod ospec-find-objects- ::bdx-from-ctx
  [fm {:keys [from-ctx to-ctx]} id]
  (for [bdxid (incident-bdx-to fm id)
        :when (bound-from-elem-in? fm bdxid from-ctx)]
    bdxid))

(defmethod ospec-find-objects- ::desideratum
  [fm {::keys [ospecs]} id]
  (apply concat
    (for [ospec ospecs
          :when (not (preference? ospec))]
      (ospec-find-objects fm ospec id))))

(defmethod ospec-find-objects- ::preference
  [fm _ id]
  nil)

(declare m-desideratum-objects)

(defmethod ospec-find-objects- ::keyword
  [fm ospec id]
  (cond
    :let [f (get-in m-desideratum-objects [ospec :find-all-fn])]
    (or (nil? f) (= unimplemented f))
      (throw (IllegalArgumentException. (<< "No :find-all-fn is defined in "
        "m-desideratum-objects for ~{ospec}.")))
    (f fm id)))

(defn ospec-find-objects
  "Returns lazy seq of all elemids in fm that match ospec (an \"object
  specification\") with respect to id. You can pass a desideratum in
  place of ospec. Ignores preferences."
  [fm ospec id]
  (if (preference? ospec)
    nil
    (ospec-find-objects- fm ospec id)))

(defmulti ospec-match?
  (fn [fm ospec thisid thatid]
    (or (and (keyword? ospec) ::keyword)
        (and (contains? ospec ::ospecs) ::contains-ospecs)
        (::ospec-type ospec)
        ospec)))

(defmethod ospec-match? ::keyword
  [fm ospec thisid thatid]
  (cond
    :let [f? (get-in m-desideratum-objects [ospec :match?-fn])]
    (or (nil? f?) (= unimplemented f?))
      (throw (IllegalArgumentException. (<< "No :match?-fn is defined in "
        "m-desideratum-objects for ~{ospec}.")))
    (f? fm thisid thatid)))

(defmethod ospec-match? ::contains-ospecs
  [fm {::keys [ospecs]} thisid thatid]
  (some #(ospec-match? fm % thisid thatid) ospecs))

(defn leaf-node-ospecs [ospec]
  (if (and (map? ospec) (contains? ospec ::ospecs))
    (mapcat leaf-node-ospecs (::ospecs ospec))
    [ospec]))

(defn preference-ospecs
  "Returns lazy seq of leaf-node ospecs that are preferences."
  [ospec]
  (cond
    (preference? ospec)
      (leaf-node-ospecs ospec)
    (and (map? ospec) (contains? ospec ::ospecs))
      (mapcat preference-ospecs (::ospecs ospec))
    nil))

(defn nonpreference-ospecs
  "Returns lazy seq of leaf-node ospecs that are not preferences."
  [ospec]
  (cond
    (preference? ospec)
      nil
    (and (map? ospec) (contains? ospec ::ospecs))
      (mapcat nonpreference-ospecs (::ospecs ospec))
    [ospec]))

(defn preferences [ospec]
  (cond
    (preference? ospec)
      [ospec]
    (keyword? ospec)
      nil
    (contains? ospec ::ospecs)
      (mapcat preferences (get ospec ::ospecs))
    nil))

#_(defn ospec-preferred-objects
  "Same as find-objects but only returns seq of objects within subset that
  match the object-specs within (prefer ...) clauses."
  [fm ospec subset thisid]
  (cond
    (preference? ospec)
      (filter #(ospec-match? fm ospec thisid %) subset)
    (keyword? ospec)
      nil
    (contains? ospec ::ospecs)
      (let [preference-ospecs (dd ospec (preferences ospec))]
        (filter (fn [thatid]
                  (some (fn [osp] (dd (::ospec-type osp) thisid thatid (ospec-match? fm osp thisid thatid)))
                        preference-ospecs))
                subset))
    nil))

(defmulti ospec-start-actions
  (fn [fm ospec id]
    (or (and (keyword? ospec) ::keyword)
        (and (contains? ospec ::ospecs) ::contains-ospecs)
        (::ospec-type ospec)
        ospec)))

(defmethod ospec-start-actions ::keyword
  [fm ospec id]
  (cond
    :let [start-f (get-in m-desideratum-objects [ospec :start-fn])]
    (or (nil? start-f) (= unimplemented start-f))
      (throw (IllegalArgumentException. (<< "No :start-fn is defined in "
        "m-desideratum-objects for ~{ospec}.")))
    (start-f id)))

(defmethod ospec-start-actions ::contains-ospecs
  [fm {::keys [ospecs]} id]
  (->> ospecs
       (remove preference?)
       (mapcat #(ospec-start-actions fm % id))))

(defmethod ospec-start-actions ::bdx-to-ctx
  [fm {::keys [from-ctx to-ctx]} id]
  [[:start-bdx-into-ctx-for id to-ctx]])

(defmethod ospec-start-actions ::bdx-from-ctx
  [fm {::keys [from-ctx to-ctx]} id]
  [[:start-bdx-from-ctx-for id from-ctx]])

;;; Support edges

(defn support-from
  "Returns id of supporting element."
  [fm suppid]
  (->> (g/edge->incident-ports fm suppid)
       (filter #(= :support-to (second %)))
       ffirst))

(defn support-to
  "Returns id of supported element, i.e. the supportee."
  [fm suppid]
  (->> (g/edge->incident-ports fm suppid)
       (filter #(= :support-from (second %)))
       ffirst))

;;; Printing

(defn ffmt [n]
  (format "%4.3f" (float n)))

(declare eid)

(defn expanded-id
 ([fm id]
  (expanded-id fm id false))
 ([fm id force-brackets?]
  (let [bk1 (if force-brackets? \[ "")
        bk2 (if force-brackets? \] "")]
    (cond
      (bdx? fm id)
        (str bk1 (eid fm (bound-from fm id) :force-brackets)
             " ↦ "
             (eid fm (bound-to fm id) :force-brackets) bk2)
      (support? fm id)
        (str bk1 (eid fm (support-from fm id) :force-brackets)
             " s-> "
             (eid fm (support-to fm id) :force-brackets) bk2)
      nil))))

(defn eid
 ([fm id]
  (eid fm id false))
 ([fm id force-brackets?]
  (if-let [ei (expanded-id fm id force-brackets?)]
    ei
    (str id))))
      
(defn nice-id [fm id]
  (if-let [ei (expanded-id fm id)]
    (str id \space ei)
    (str id)))

(defn pprint-edge? [fm edgeid]
  (and (not (support? fm edgeid))
       (not (bdx? fm edgeid))))

(defn pprint-edges [fm]
  (cond
    :let [edgeids (filter #(pprint-edge? fm %) (g/edges fm))]
    (empty? edgeids)
      (println "Edges: None")
    :do (do
          (println "Edges:")
          (doseq [s (->> edgeids (map #(g/edgestr fm %)) sort)]
            (println \space s)))))

(defn bdxstr [fm bdxid]
  (str (nice-id fm bdxid) \space
       (util/map-str
         (select-keys (g/attrs fm bdxid)
                      [:prev-total-support :self-support :total-support]))))

(defn pprint-bdx [fm]
  (cond
    :let [ids (all-bdx fm)]
    (empty? ids)
      (println "Bindings: None")
    (do
      (println "Bindings:")
      (doseq [s (->> ids (map #(bdxstr fm %)) sort)]
        (println \space s)))))

(defn parsed-support-deltas [fm]
  (for [delta (:support-deltas fm)]
    (pmatch delta
      (:add-support ~fromid ~toid ~amt)
        [fromid toid amt])))

(defn totaled-support-deltas
  "Returns a map {fromid {toid amt}} where amt is the sum of all the
  support given in :support-deltas from fromid to toid."
  [fm]
  (with-state [m {}]
    (doseq [[fromid toid amt] (parsed-support-deltas fm)]
      (update-in [fromid toid] (fnil + 0.0) amt))))

(defn suppinfo
  "Returns a map {:fromid fromid, :toid toid, :attempted-weight
  attempted-weight, :prev-weight prev-weight, :weight weight}."
  [fm suppid]
  {:fromid (support-from fm suppid)
   :toid (support-to fm suppid)
   :prev-weight (g/attr fm suppid :prev-weight)
   :attempted-weight (g/attr fm suppid :attempted-weight)
   :weight (g/weight fm suppid)})

(defn supportstr [fm suppid]
  (let [{:keys [fromid toid] :as m} (suppinfo fm suppid)]
    (format "%s  %-20s %s" suppid (eid fm suppid)
      (util/map-str (select-keys m [:prev-weight :attempted-weight :weight])))))

(defn support-totals-str [fm]
  (str 
    "(total :self-support " (ffmt (total-self-support fm)) ") "
    "(total :total-support " (ffmt (total-total-support fm)) ")"))

(defn pprint-support [fm]
  (cond
    :let [ids (all-support-edges fm)]
    (empty? ids)
      (println (str "Support: None  " (support-totals-str fm)))
    (let [total (total-of-all-support-weights fm)
          postotal (total-of-all-pos-support-weights fm)]
      (println (str
        "Support:     (total :weight " (ffmt total)
        ") (total pos :weight " (ffmt postotal) ") "
        (support-totals-str fm)))
      (doseq [s (->> ids (map #(supportstr fm %)) sort)]
        (println \space s)))))

(defn pprint-model [fm]
  (println "Gattrs:" (util/map-str (g/gattrs fm)))
  (g/pprint-nodes fm)
  (pprint-edges fm)
  (pprint-bdx fm)
  (pprint-support fm))

(defn print-model-state [fm]
  #_(g/pprint fm)
  (pprint-model fm)
  fm)

;;; dot

(def dot-preamble
"digraph g {
  graph [layout=\"fdp\"]
  nodesep=1.5
  node [penwidth=0 fontname=\"Friz Quadrata\"];\n")

(def bdxcolor "olivedrab")
(def pos-suppcolor "cadetblue2")
(def neg-suppcolor "tomato")
(def tagcolor "maroon4")

;HACK
(def ok-port-labels [:result])

;HACK
(def hard-coded
  {:brick "pos=\"0,0!\""     ;4
   :brick002 "pos=\"3,0!\""  ;5
   :brick003 "pos=\"6,0!\""  ;6
   :block "pos=\"1.5,6!\""     ;9
   :target "pos=\"3,9!~\""   ;15
   :plus002 "pos=\"1.5,3!\""
   :plus003 "pos=\"4.5,7.5!\""

   :number002 "pos=\"12,0!\"" ;5
   :number003 "pos=\"15,0!\"" ;6
   :number "pos=\"13.5,6!\"" ;11
   :plus "pos=\"13.5,3!\""
  })

;HACK
(def skip #{:problem :eqn})

(defn tag-edges->dot [fm tagid]
  (with-out-str
    (let [i (name tagid)]
      (doseq [id (g/port->neighbors fm [tagid :from])]
        (println (<< "  ~{i} -> ~(name id) [color=~{tagcolor}];")))
      (doseq [id (g/port->neighbors fm [tagid :to])]
        (println (<< "  ~{i} -> ~(name id) [color=~{tagcolor}];"))))))

(defn normal-edges->dot [fm elemid]
  (with-out-str
    (let [i (name elemid)]
      (doseq [pl ok-port-labels
              thatid (g/port->neighbors fm [elemid pl])]
        (let [ti (name thatid)]
          (println (<< "  ~{i} -> ~{ti};")))))))

(defn node->dot [fm nodeid]
  (cond
    :let [as (g/attrs fm nodeid)
          i (name nodeid)
          n (:n as)]
    (some? n)
      (<< "  ~{i} [label=\"~{n}\" ~(hard-coded nodeid)];\n"
          "~(normal-edges->dot fm nodeid)")
    :let [letter (:letter as)]
    (some? letter)
      (<< "  ~{i} [label=\"~{letter}\" ~(hard-coded nodeid)];\n")
    :let [cl (:class as)]
    (= :operator cl)
      (<< "  ~{i} [label=\"+\" ~(hard-coded nodeid)];\n"  ;HACK
          "~(normal-edges->dot fm nodeid)")
    (= :tag cl)
      (<< "  ~{i} [label=\"~(:tagclass as)\" fontcolor=~{tagcolor} ~(hard-coded nodeid)];\n"
          "~(tag-edges->dot fm nodeid)")
    (some? cl)
      (<< "  ~{i} [label=\"~{cl}\" ~(hard-coded nodeid)];\n"
          "~(normal-edges->dot fm nodeid)")
    (<< "  ~{i}\n")))

(defn bdx->dot [fm bdxid]
  (let [i (name bdxid)
        ifrom (name (bound-from fm bdxid))
        ito (name (bound-to fm bdxid))
        weight (max 0.1 (* 8.0 (total-support-for fm bdxid)))]
  (<< "  ~{i} [label=\"bind\" fontcolor=~{bdxcolor}];\n"
      "  ~{ifrom} -> ~{i} [penwidth=~{weight} color=~{bdxcolor} "
         \] ;"arrowhead=none];\n"
      "  ~{i} -> ~{ito} [penwidth=~{weight} color=~{bdxcolor}];\n")))

(defn supp->dot [fm suppid]
  (let [i (name suppid)
          ifrom (name (support-from fm suppid))
          ito (name (support-to fm suppid))
          w (g/weight fm suppid)
          color (if (>= w 0.0) pos-suppcolor neg-suppcolor)
          weight (max 0.1 (* 8.0 (Math/abs w)))]
    (<< "  ~{ifrom} -> ~{ito} [penwidth=~{weight} color=~{color}];\n")))

(defn fm->dot [fm]
  (with-out-str
    (print dot-preamble)
    (doseq [nodeid (->> (g/nodes fm) (remove skip))]
      (print (node->dot fm nodeid)))
    (doseq [bdxid (all-bdx fm)]
      (print (bdx->dot fm bdxid)))
    (doseq [suppid (all-support-edges fm)]
      (print (supp->dot fm suppid)))
    (println "}")
  ))

;;; Actions

(def m-desideratum-objects
  {:bdx
    {:start-fn (fn [id] [[:start-bdx-for id]])
     :find-all-fn incident-bdx
     :match?-fn bound-from-or-to?}
   :bdx-from
    {:start-fn (fn [id] [[:start-bdx-from-for id]])
     :find-all-fn incident-bdx-from
     :match?-fn bound-from?}
   :adj-bdx
    {:start-fn unimplemented
     :find-all-fn (fn [fm id] (filter #(edge-to-adjacent? fm id %)
                                      (incident-bdx fm id)))
     :match?-fn (fn [fm id bdxid]
                  (and (bound-from-or-to? fm id bdxid)
                       (edge-to-adjacent? fm id bdxid)))}
   :basis
    {:start-fn unimplemented
     :find-all-fn (fn [fm id] (g/port->neighbors fm [id :basis]))
     :match?-fn unimplemented}
   :basis-of
    {:start-fn unimplemented
     :find-all-fn (fn [fm id] (g/port->neighbors fm [id :basis-of]))
     :match?-fn unimplemented}
   :mates-to-exist
    {:start-fn (constantly nil)
     :find-all-fn (fn [fm bdxid] (g/edge->incident-elems fm bdxid))
     :match?-fn unimplemented}
   :other-bdx-from-same-mate
    {:start-fn unimplemented
     :find-all-fn (fn [fm fromid] (->> (bound-from fm fromid)
                                       (incident-bdx-from fm)
                                       (remove= fromid)))
     :match?-fn unimplemented}
   :bindbacks
    {:start-fn unimplemented
     :find-all-fn bindbacks
     :match?-fn unimplemented}
   :bad-nbr-bdx
    {:start-fn unimplemented
     :find-all-fn bad-nbr-bdx
     :match?-fn unimplemented}
   :bdx-to-different-number
    {:start-fn unimplemented
     :find-all-fn bdx-to-different-number
     :match?-fn unimplemented}
   :has-basis
    {:start-fn unimplemented
     :find-all-fn unimplemented
     :match?-fn (fn [fm _ id]
                  (not (empty? (g/port->incident-edges fm [id :basis]))))}})

(defn ospec-unsatisfied? [fm ospec id]
  (if (keyword? ospec)
    ;(empty? ((get-in m-desideratum-objects [ospec :find-all-fn]) fm id))
    (empty? (ospec-find-objects fm ospec id))
    false))

(defn desideratum->actions
  "Returns seq of actions, or nil if none."
  [fm id desideratum]
  (case (get desideratum ::desideratum-type)
    ::need
      (when (empty? (ospec-find-objects fm desideratum id))
        (ospec-start-actions fm desideratum id))
    nil)
  #_(pmatch desideratum
    (:need ~@ospecs)
      (for [ospec ospecs
            :when (ospec-unsatisfied? fm ospec id)]
        ((get-in m-desideratum-objects [ospec :start-fn]) id))
    ~any
      nil))

(declare try-to-tag-for)

(defn add-orientation-for [fm id & other-ids]
  (with-state [fm fm]
    (doseq [other-id other-ids]
      (try-to-tag-for other-id))))

(defn do-action [fm action]
  (pmatch action
    (:start-bdx-for ~id)
      (start-bdx-for fm id)
    (:start-bdx-from-for ~id)
      (start-bdx-from-for fm id)
    (:start-bdx-into-ctx-for ~id ~to-ctx)
      (start-bdx-into-ctx-for fm id to-ctx)
    (:start-bdx-from-ctx-for ~id ~from-ctx)
      (start-bdx-from-ctx-for fm id from-ctx)
    (:request-orientation-for ~id ~toids)
      (apply add-orientation-for fm id toids)))

(defn do-actions [fm]
  (reduce #(do-action %1 %2) fm (:actions fm)))

(defn do-queued-actions [fm]
  (reduce #(do-action %1 %2) fm (:queued-actions fm)))

(defn post-actions-for-desiderata [fm]
  (with-state [fm fm]
    (doseq [node (g/nodes fm)
            desideratum (g/attr fm node :desiderata)]
      (update :actions into (desideratum->actions fm node desideratum)))))

;(defn attempted-weights-from [fm fromid]
;  (map #(g/attr fm % :attempted-weight)
;       (support-edges-from fm fromid)))
;
;(defn something-is-salient?
;  "Something is salient if the max element of coll is at least 10% greater
;  than the second-to-max element."
;  [coll]
;  (cond
;    :let [top2 (vec (max2 coll))]
;    (= 1 (count top2))
;      true
;    (empty? top2)
;      false
;    :let [x1 (top2 0), x2 (top2 1)]
;    (>= x1 (+ x2 (* 0.1 x2)))))

(defn nonsalient-supportees-of [fm fromid]
  (cond
    :let [pairs (->> (outgoing-support-edges fm fromid)
                     (map (fn [suppid]
                            [suppid (attempted-weight-of fm suppid)]))
                     (sort-by (comp - second)))]
    (or (empty? pairs) (= 1 (count pairs)))
      nil
    :let [max-attempted-weight (-> pairs first second)
          threshold (* 0.9 max-attempted-weight)
          top-supportees (->> pairs
                              (take-while #(> (second %) threshold))
                              (map #(support-to fm (first %))))]
    (< (count top-supportees) 2)
      nil
    top-supportees))

(defn queue-actions [fm]
  (if (:request-orientation? fm)
    (with-state [fm fm]
      (doseq [[fromid _] (->> (:support-deltas fm)
                              (filter #(not (bdx? fm (first %)))))]
        (bind toids (nonsalient-supportees-of fm fromid))
        (when (not (empty? toids))
          (update :queued-actions conj
                  [:request-orientation-for fromid toids]))))
    fm))

;;; Support for desiderata

(defn add-support
 ([fm fromid toid]
  (add-support fm fromid toid (need-delta fm)))
 ([fm fromid toid amt]
  (assert (>= amt 0.0))
  (update-in fm [:support-deltas fromid toid]
    (fn [old-delta]
      (cond
        (nil? old-delta)
          amt
        (< old-delta 0.0)
          old-delta  ; don't add support if fromid has antipathy to toid
        (+ old-delta amt))))
  ))

(defn add-antipathy
 ([fm fromid toid]
  (add-antipathy fm fromid toid (antipathy-per-timestep fm)))
 ([fm fromid toid amt]
  (assert (<= amt 0.0))
  (update-in fm [:support-deltas fromid toid]
    (fn [old-delta]
      (cond
        (or (nil? old-delta)
            (>= old-delta 0.0))
          amt ;replace support with antipathy
        ;add to existing antipathy
        (+ old-delta amt))))))

#_(defn find-objects
  "ospecs is the part of a desideratum after the verb, which lists
  types of objects to support or oppose. The inside of a (:prefer ...)
  clause is also an objects-spec. Returns seq of all matching objects."
  [fm fromid ospecs]
  (apply concat
    (for [ospec ospecs]
      (pmatch ospec
        ~kw (guard (keyword? kw))
          (let [find-all (get-in m-desideratum-objects [kw :find-all-fn])]
            (find-all fm fromid))
        ~ls (guard (or (sequential? ls) (nil? ls)))
          nil  ; ignore preferences
        ))))

#_(defn matching-objects
  [fm fromid subset object-spec]
  (let [match? (get-in m-desideratum-objects [object-spec :match?-fn])]
    (assert match?
      (<< "matching-objects: ~{fromid}'s object-spec ~{object-spec} has "
          "no :match?-fn."))
    (filter #(match? fm fromid %) subset)))

#_(defn preferred-objects
  "Same as find-objects but only returns seq of objects within subset that
  match the object-specs within (:prefer ...) clauses. The objects-spec
  argument should be the whole list of desiderata, not an individual
  preference clause."
  [fm fromid subset ospecs]
  (apply concat
    (for [ospec ospecs]
      (pmatch ospec
        (:prefer ~@preferred-object-specs)
          (apply concat
            (for [ospec preferred-object-specs]
              (matching-objects fm fromid subset ospec)))
        ~any
          nil))))

(defn matching-objects
  [fm ospec subset thisid]
  (filter #(ospec-match? fm ospec thisid %) subset))

(defn preferred-objects
  "Returns the same object as many times as the number of preferences
  that it meets."
  [fm ospec subset thisid]
  (apply concat
    (for [po (preference-ospecs ospec)]
      (matching-objects fm po subset thisid))))

(defn post-support-for-desideratum
  "Adds/subtracts to :support-deltas for one desideratum."
  [fm fromid desideratum]
  (case (get desideratum ::desideratum-type)
    ::need
      (let [needed (ospec-find-objects fm desideratum fromid) #_(find-objects fm fromid ospecs)
            preferred (preferred-objects fm desideratum needed fromid) #_(preferred-objects fm fromid needed ospecs)]
        (with-state [fm fm]
          (doseq [toid needed]
            (add-support fromid toid))
          (doseq [toid preferred]
            (add-support fromid toid (preference-delta fm)))))
    ::want
      (with-state [fm fm]
        (doseq [toid (ospec-find-objects fm desideratum fromid) #_(find-objects fm fromid ospecs)]
          (add-support fromid toid)))
    ::oppose
      (let [fromid-prev (prev-total-support-for fm fromid)]
        (with-state [fm fm]
          (doseq [toid (ospec-find-objects fm desideratum fromid) #_(find-objects fm fromid ospecs)]
            (add-antipathy fromid toid))))))

(defn post-support-deltas-for-desiderata [fm]
  (with-state [fm fm]
    (doseq [id (elems-that-can-give-support fm)
            desideratum (g/attr fm id :desiderata)]
      (post-support-for-desideratum id desideratum))))

(defn post-positive-feedback-for-support [fm]
  (with-state [fm fm]
    (doseq [fromid (elems-that-can-give-support fm)
            supportee (supportees-of fm fromid)]
      (bind supportee-support (prev-total-support-for fm supportee))
      (when (pos? supportee-support)
        (bind amt (* (positive-feedback-rate fm) supportee-support))
        (when (not (close-to-zero? amt))
          (add-support fromid supportee amt))))))

;;; Tagging

(defn tags-of
 ([fm elem]
  (g/port->neighbors fm [elem :tags]))
 ([fm tagclass elem]
  (->> (g/port->neighbors fm [elem :tags])
       (filter #(= tagclass (tagclass-of fm %))))))

(defn tagports-of
 ([fm elem]
  (g/port->neighboring-ports fm [elem :tags]))
 ([fm tagclass elem]
  (->> (g/port->neighboring-ports fm [elem :tags])
       (filter (fn [[tagid port-label]]
                 (= tagclass (tagclass-of fm tagid)))))))

(defn tags-of-via-port-label
  "Returns set of all tags on elem, with :class tagclass, connected to
  elem from [tag port-label]."
  [fm tagclass port-label elem]
  (let [tports (if (= :any tagclass)
                 (tagports-of fm elem)
                 (tagports-of fm tagclass elem))]
    (set
      (for [[tagid tag-port-label] tports
            :when (= port-label tag-port-label)]
        tagid))))

(defn tags-of2
  "Returns set of tags with given taggees and given tagclass."
  [fm tagclass & taggees]
  (assert (even? (count taggees)))
  (let [tagss (map (fn [[port-label elem]]
                     (tags-of-via-port-label fm tagclass port-label elem))
                   (partition 2 taggees))]
    (apply clojure.set/intersection tagss)))

(defn has-tag?
  "Returns set of tags of given tagclass between given taggees, or nil if
  none. taggees has same format same as in add-tag."
  [fm tagclass & taggees]
  (let [tags (apply tags-of2 fm tagclass taggees)]
    (if (empty? tags)
      nil
      tags)))

(defn add-basis-edge [fm basis basis-of]
  (g/add-edge fm nil [basis :basis-of] [basis-of :basis]))

(defn add-basis-from-tag [fm tagid]
  (with-state [fm fm]
    (bind tagfrom (g/port->neighbor fm [tagid :from]))
    (bind tagto (g/port->neighbor fm [tagid :to]))
    (doseq [bdxid (bdx-from-to fm tagfrom tagto)]
      (add-basis-edge tagid bdxid))))

(defn add-basis-from-bdx [fm bdxid]
  (with-state [fm fm]
    (bind bdxfrom (bound-from fm bdxid))
    (bind bdxto (bound-to fm bdxid))
    (doseq [tagid (tags-of2 fm :any :from bdxfrom :to bdxto)]
      (add-basis-edge tagid bdxid))))

(defn add-tag
  "taggees is any number of: port-label node. An edge will be created for
  each taggee, from [tag port-label] to [node :tags]. If the tag already
  exists, a new one will not be created."
  [fm tagclass & taggees]
  (assert even? (count taggees))
  (if (not (apply has-tag? fm tagclass taggees))
    (with-state [fm fm]
      (setq tagid (g/make-node :tag {:class :tag
                                     :tagclass tagclass
                                     ;:desiderata #{[:want :basis-of]}
                                     :desiderata #{(want :basis-of)} ;NEW WAY
                                     :self-support [:decaying 0.2]
                                     :total-support nil}))
      (doseq [[port-label taggee] (partition 2 taggees)]
        -- (assert (g/has-node? fm taggee))
        (g/add-edge nil [tagid port-label] [taggee :tags]))
      (add-basis-from-tag tagid))
    fm))

;TODO Data for the logic here needs to go into the spec.
(defn try-to-tag [fm id1 id2]
  (cond
    (= (g/attr fm id1 :adj-right) id2)
      (add-tag fm :adj-right :from id1 :to id2)
    (= id1 (g/attr fm id2 :adj-right))
      (add-tag fm :adj-right :from id2 :to id1)
    (same-number? fm id1 id2)
      (add-tag fm :same-number :from id1 :to id2)
    (different-number? fm id1 id2)
      (add-tag fm :different-number :from id1 :to id2)
    fm))

;TODO Data for the logic here needs to go into the spec.
(defn try-to-tag-for [fm id]
  (case (class-of fm id)
    :bind
      (try-to-tag fm (bound-from fm id) (bound-to fm id))
    fm))

;;; Updating support weights

(defn decay-self-support [fm elem]
  (pmatch (g/attr fm elem :self-support)
    [:decaying ~n]
      (g/set-attr fm elem :self-support [:decaying (* 0.9 n)])
    ~any
      fm))

(defn delta [fm fromid toid]
  (get-in fm [:support-deltas fromid toid] 0.0))

(defn prev-weight [fm elemid]
  (cond
    :let [pw (g/attr fm elemid :prev-weight)]
    (nil? pw)
      0.0
    pw))

(defn calculate-attempted-weight
  "Sets :attempted-weight of :support edge from fromid to toid. Makes edge
  if needed."
  [fm fromid toid]
  (with-state [fm fm]
    (setq edgeid (find|make-support-edge fromid toid))
    (bind prevts (prev-total-support-for fm fromid))
    (g/set-attr edgeid :attempted-weight 
      (cond
        (<= prevts 0.0)
          0.0
        :let [d (delta fm fromid toid)]
        (neg? d)
          (max (- prevts) d)
        (min prevts
             (+ (prev-weight fm edgeid)
                (* prevts d)))))))

(defn calculate-attempted-weights [fm]
  (with-state [fm fm]
    (doseq [[fromid m] (:support-deltas fm)
            toid (keys m)]
      (calculate-attempted-weight fromid toid))))

(defn all-attempted-weights [fm]
  (for [edgeid (all-support-edges fm)]
    (g/attr fm edgeid :attempted-weight)))

(defn calculate-weights [fm]
  (let [target-sum (total-support-available fm)
        m (into {} (support-edges-and-attempted-weights fm))
        new-m (cnormalize-vals fm target-sum m)
        ;expt (normalization-expt fm)
        ;new-m (expt-scale-down-vals expt target-sum m)
        ]
    (with-state [fm fm]
      (doseq [[suppid weight] new-m]
        (g/set-attr suppid :weight weight)))))

;;; Total support

(defn save-prevs [fm]
  (with-state [fm fm]
    (doseq [elem (elems-that-can-receive-support fm)]
      (g/set-attr elem :prev-total-support (g/attr fm elem :total-support))
      (g/set-attr elem :prev-self-support (self-support-for fm elem)))
    (doseq [suppid (all-support-edges fm)]
      (g/set-attr suppid :prev-weight (g/attr fm suppid :weight))
      (g/set-attr suppid :attempted-weight (g/attr fm suppid :weight)))))

(defn normalize-total-supports [fm]
  (let [support-limit (total-support-available fm)
        m (into {} (support-edges-and-weights fm))
                     ;Is this right? Neg weights count negatively.
        expt (normalization-expt fm)]
    (with-state [fm fm]
      (doseq [[suppid weight] (cnormalize-vals fm support-limit m)]
        (g/set-attr suppid :weight weight)))))

(defn calculate-total-support [fm toid]
  (g/set-attr fm toid :total-support
    (+ (self-support-for fm toid)
       (reduce + (incoming-support-weights fm toid)))))

(defn calculate-total-supports [fm]
  (with-state [fm fm]
    (doseq [toid (elems-that-can-receive-support fm)]
      (calculate-total-support toid))
    (when (:normalize-total-supports? fm)
      normalize-total-supports)))

;;; Removing graph elements

(defn remove-unsupported-elems [fm]
  (with-state [fm fm]
    (doseq [elem (elems-that-can-receive-support fm)]
      (when (and (< (self-support-for fm elem) (minimum-self-support fm))
                 (< (total-support-for fm elem) (minimum-total-support fm)))
        (g/remove-elem elem)))))

;;; Saving observations

(defn save-obs [fm]
  (let [t (:timestep fm)]
    (doseq [id (elems-that-can-receive-support fm)]
      (obs :support {"t" t
                     "id" (nice-id fm id)
                     "support" (total-support-for fm id)}))
    (doseq [suppid (all-support-edges fm)]
      (let [{:keys [fromid toid weight]} (suppinfo fm suppid)]
        (obs :weight {"t" t
                      "suppid" (str suppid \space fromid " -> " toid)
                      "weight" (g/weight fm suppid)})))
    (obs :totals {"t" t
                  "total" "pos-weight"
                  "y" (total-of-all-pos-support-weights fm)})
    (obs :totals {"t" t
                  "total" "self-support"
                  "y" (total-self-support fm)})
    (obs :totals {"t" t
                  "total" "total-support"
                  "y" (total-total-support fm)}))
  fm)

;;; Running

(defn do-scheduled [fm]
  (if-let [f (get-in fm [:scheduled (get-timestep fm)])]
    (f fm)
    fm))

(defn do-timestep [fm]
  (with-state [fm fm]
    (update :timestep inc)
    (when (not (:quiet? fm))
      -- (println "\n---------- timestep" (:timestep fm) "-----------\n"))
    (assoc :actions #{}, :support-deltas {})

    save-prevs
    (doseq [elem (elems-that-can-receive-support fm)]
      (decay-self-support elem))
    remove-unsupported-elems
    do-queued-actions
    (assoc :queued-actions #{})
    do-scheduled
    post-actions-for-desiderata
    do-actions
    post-support-deltas-for-desiderata
    post-positive-feedback-for-support
    calculate-attempted-weights
    calculate-weights
    calculate-total-supports
    queue-actions
    save-obs))

(defn run-
  "params is an optional map with overrides to default-globals. Runs (:map
  params), or the abc model if not specified. Returns the state of the model
  (a pgraph) after :timesteps timesteps."
 ([]
  (run- {}))
 ([params]
  (let [model (get params :model abc)
        params (merge {:timesteps 100} (dissoc params :model))]
    (log/reset)
    (clear-data!)
    (with-logging (:log params)
      (with-state [fm (start-model model params)]
        calculate-total-supports
        save-obs
        (when (not (:quiet? fm))
          print-model-state)
        (dotimes [t (:timesteps params)]
          do-timestep
          ;-- (println)
          (when (not (:quiet? fm))
            print-model-state))
        -- (write-data)
        (when (:quiet? fm)
          print-model-state))))))

(defn run [& {:keys [] :as params}]
  (run- params))

;;; Tests of the model

(defn run-model-test [base-params overrides expectf]
  (let [[overrides expectf] (if (nil? overrides)
                              [{:quiet? true} expectf]
                              [overrides (constantly true)])
        fm (run- (merge base-params overrides))]
    (assert (expectf fm))
    fm))

(defn results [fm]
  (->> (elems-that-can-give-support fm)
       (map (fn [id] [id (total-support-for fm id)]))
       (into {})))

(defn top-bindings [fm]
  (->> (all-bdx fm)
       (sort-by #(- (total-support-for fm %)))))

(defn expect-top-bindings
  "Returns a Boolean function suitable for passing as the expectf argument
  to run-model-test. That function returns logical true iff the most-supported
  n bindings are those listed symbolically in bdxs, where n is the length of
  bdxs. Each element of bdxs should look like this: [:bind a b]."
  [bdxs]
  (fn [fm]
    (=sets bdxs (->> (top-bindings fm)
                     (take (count bdxs))
                     (map #(symbolically fm %))))))

(defn expect-only-bindings
  "Returns a Boolean function like expect-top-bindings, but it checks that
  that the bindings in the model are the same as those passed to the
  function (i.e. the model has all of 'bdxs' and no other bindings)."
  [bdxs]
  (fn [fm]
    (=sets bdxs (all-bdx-symbolically fm))))

(def test1-params
  {:need-delta 0.1
   :preference-delta 0.1
   :antipathy-per-timestep -0.5
   :positive-feedback-rate 0.1
   :minimum-total-support 0.02
   :minimum-self-support 0.02
   :normalization-expt 1.5
   :normalize-total-supports? true
   :cnormalize-method :by-exponent
   :request-orientation? false
   :init #(g/set-attr % 'a :self-support [:permanent 1.1])})

(defn test1
  "Should end with a->b and b->c dominant, maybe a little bit for c->a, and
  no other bindings.

  This test fails if :request-orientation? is true. Not sure why."
  [& {:keys [] :as overrides}]
  (run-model-test test1-params overrides
    (expect-top-bindings [[:bind 'a 'b] [:bind 'b 'c]])))

(defn mkdecaying [fm]
  (with-state [fm fm]
    (g/set-attr 'a :self-support [:decaying 1.1])
    (g/set-attr 'b :self-support [:decaying 1.0])
    (g/set-attr 'c :self-support [:decaying 1.0])))

(def test2-params
  {:need-delta 0.01
   :preference-delta 0.05
   :antipathy-per-timestep -0.2
   :normalization-expt 0.5
   :positive-feedback-rate 0.2
   :cnormalize-method :by-sigmoid
   :request-orientation? true
   :support-limit 3.0
   :init mkdecaying})

(defn test2
  "Without permanent support for anything, this arrangement behaves remarkably
  well. The bindings get one spike of support at the beginning and then sort
  themselves out, with a->b and b->c the strongest, and c->a also there; the
  others disappear. :positive-feedback-rate seems to have little effect.
  Nearly all the work is done by diag-sigmoid with an exponent set to even
  out support :weights, driving them all toward the mean. At the end,
  support is fairly equally distributed among all the surviving elems,
  with somewhat more in b than a or c, and the most in the top two bindings."
  [& {:keys [] :as overrides}]
  (run-model-test test2-params overrides
    (expect-top-bindings [[:bind 'a 'b] [:bind 'b 'c]])))

(def abcd
  (-> (graph (left-to-right-seq 'a 'b 'c 'd))
      (g/merge-default-attrs 'a letter-attrs {:letter 'a})
      (g/merge-default-attrs 'b letter-attrs {:letter 'b})
      (g/merge-default-attrs 'c letter-attrs {:letter 'c})
      (g/merge-default-attrs 'd letter-attrs {:letter 'd})))

(def test3-params (merge test2-params
  {:model abcd
   :init nil}))

(defn test3
  "abcd should end with bindings going left to right."
  [& {:keys [] :as overrides}]
  (run-model-test test3-params overrides
    (expect-top-bindings [[:bind 'a 'b]
                          [:bind 'b 'c]
                          [:bind 'c 'd]])))

(def test4-letter-attrs
  {:class :letter
   :self-support [:permanent 1.0]
   ;:desiderata #{[:need :bdx [:prefer :has-basis]]}
   :desiderata #{(need :bdx (prefer :has-basis))} ;NEW WAY
   :total-support nil})

(def test4-model
  (-> (graph (left-to-right-seq 'a 'b 'c))
      (update-nodes symbol? #(set-letter-attrs %1 %2 test4-letter-attrs))))

(def test4-params (merge test2-params
  {:model test4-model
   ;:antipathy-per-timestep -0.1
   ;:positive-feedback-rate 0.05
   :init nil
   :scheduled {2 (fn [fm] (try-to-tag fm 'a 'b))
               10 (fn [fm] (try-to-tag fm 'b 'c))}}))

(defn test4
  "We tag a,b and then b,c with :adj-right, and the symmetry is broken.
  The tags decay out of existence, and a->b, b->c, and c->a are the only
  bindings that remain at the end. a->b has the most support, followed by
  b->c and then c->a."
  [& {:keys [] :as overrides}]
  (run-model-test test4-params overrides
    (expect-only-bindings [[:bind 'a 'b]
                           [:bind 'b 'c]
                           [:bind 'c 'a]])))

(def test5-params
  (assoc test4-params :scheduled nil
                      :request-orientation? true))

(defn test5
  "Like test4, except that in test5, no tagging is scheduled. The letters
  themselves request tags to resolve lack of salience."
  [& {:keys [] :as overrides}]
  (run-model-test test5-params overrides
    (expect-only-bindings [[:bind 'a 'b]
                           [:bind 'b 'c]
                           [:bind 'c 'a]])))

(defn start-bind-across-ctxs [fm from-ctx to-ctx]
  (with-state [fm fm]
    (doseq [memberid (members-of fm from-ctx)]
      (g/update-attr memberid :desiderata conj
        (need (bdx-to-ctx from-ctx to-ctx))
        (oppose :bad-nbr-bdx))
      (when (isa-number? fm memberid)
        (g/update-attr memberid :desiderata conj
          (oppose :bdx-to-different-number))))))

(defn start-numbo-builds [fm from-ctx to-ctx]  ;HACK
  (with-state [fm fm]
    (setq new+ (g/make-node :plus
                 {:desiderata #{(need (bdx-from-ctx from-ctx to-ctx))}
                  :self-support [:decaying 0.2]
                  :total-support nil}))
    (g/add-edge nil [new+ :result] [:target15 :source])
    (g/add-edge nil [:block9 :result] [new+ :operands])
    (g/add-edge nil [:brick6 :result] [new+ :operands])))

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

(def v20-model
  (graph little-numbo-spec
    (with-node-attrs {:self-support [:permanent 1.0], :total-support nil}
      (ctx :eqn
        [:number :n 15]
        [:number :n 9]
        [:number :n 6]
        :plus
        [9 -> :plus]
        [6 -> :plus]
        [:plus -> 15])
      (ctx :problem
        [:target :n 15]
        [:block :n 9]
        [:brick :n 4]
        [:brick :n 5]
        [:brick :n 6]
        :plus
        [4 -> :plus]
        [5 -> :plus]
        [:plus -> 9]))))

;NEXT
; with-attrs
; Support for edges
; Competition for :source, :operands, and :result ports
; Build a node to link to if you can't find one
; Build an edge to link to if you can't find one and your "from" nodes
;   have mates
; Edge-to-edge bindings oppose neighboring edge-to-edge bindings that
;   don't allow paths to map to paths

;IDEA
; Max antipathy at 1/2 the object's :prev-total-support

(def test6-params (assoc test2-params
  :model v20-model
  :init nil
  :antipathy-per-timestep -0.1
  :support-limit nil
  :scheduled {1 (fn [fm] (start-bind-across-ctxs fm :eqn :problem))
              11 (fn [fm] (start-numbo-builds fm :eqn :problem))}
  ))

(defn test6
  "Repeats the main test of v20: Can we avoid getting confused by the two
  5's when binding from the slipnet structure 5+6=11 to the workspace
  4+5=9,6,15?"
  [& {:keys [] :as overrides}]
  (run-model-test test6-params overrides
    (constantly true))
  )

(defn demo
  "Run this to see what farg.model1 does."
  []
  (run :timesteps 3 :model abc))
