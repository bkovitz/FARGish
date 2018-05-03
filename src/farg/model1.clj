(ns farg.model1
  "Very simple FARG model: just make internal bindings in 'abc'."
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [com.rpl.specter :as S]
            [farg.logging :as log :refer [with-logging log logdd logdo]]
            [farg.gui :as gui]
            [farg.graphs2 :as g :refer [graph make-graph]]
            [farg.no-reload :refer [gui-state]]
            [farg.pmatch :refer [pmatch]]
            [farg.util :as util :refer [dd remove= find-first with-rng-seed
              mapvals]]
            [farg.with-state :refer [with-state]]))

; TODO
; parameterize demo

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

(defn all-nodes-other-than [fm id]
  (->> (g/nodes fm) (remove= id)))

(defn class-of [fm id]
  (g/attr fm id :class))

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

;;; Global parameters (these belong in the FARG model itself)

(def need-delta 0.1)
(def preference-delta 0.1)
(def antipathy-per-timestep -0.5)
(def positive-feedback-rate 0.1)
(def minimum-total-support 0.02)
(def minimum-self-support 0.02)
(def normalization-expt 1.5)
  ; Applied when normalizing a set of values whose sum exceeds some limit.
  ; Must be > 1.0 for higher values to drive down lower values.

(defn raise-to-norm-expt [n]
  (cond
    (> n 0.0)
      (Math/pow n normalization-expt)
    (< n 0.0)
      (- (Math/pow (- n) normalization-expt))
    0.0))

(defn expt-scale-down-vals
 ([target-sum m]
  (expt-scale-down-vals target-sum +abs m))
 ([target-sum sumf m]
  (cond
    (empty? m)
      m
    :let [vs (vals m)
          sum (reduce + vs)]
    (or (<= sum target-sum) (zero? sum))
      m
    :let [new-vs (map raise-to-norm-expt vs)
          sum (reduce sumf new-vs)
          scaling-factor (/ target-sum sum)]
    (zipmap (keys m)
            (map #(* scaling-factor %) new-vs)))))

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
  (filter #(not (support? fm %)) (g/elems fm)))

(def elems-that-can-receive-support elems-that-can-give-support)

(defn find-support-edge
  "Returns nil if :support edge does not exist."
  [fm fromid toid]
  (g/find-edgeid fm [fromid :support-to] [toid :support-from]))

(defn remove-support-edge [fm fromid toid]
  (g/remove-edge fm [fromid :support-to] [toid :support-from]))

(defn add-support-edge [fm fromid toid weight]
  (g/add-edge fm [fromid :support-to] [toid :support-from]
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

(defn self-support-for [fm elem]
  (pmatch (g/attr fm elem :self-support)
    (:permanent ~n)
      n
    (:decaying ~n)
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
  (->> fm
       elems-that-can-give-support
       (map #(self-support-available-from fm %))
       (reduce +)))

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
       (reduce +)))

(defn total-of-all-pos-support-weights [fm]
  (->> (all-support-edges fm)
       (map #(g/weight fm %))
       (filter pos?)
       (reduce +)))

(defn permanent-support-for [fm id]
  (pmatch (g/attr fm id :self-support)
    (:permanent ~n)
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
   :self-support `(:permanent 1.0)
   :desiderata #{`(:need :bdx (:prefer :bdx-from :adj-bdx))}
   ;:desiderata #{:want-bdx :want-adj-bdx}
   ;:desiderata #{:want-bdx-from}
   ;:desiderata #{:want-bdx}
   })

(def abc
  (-> (graph (left-to-right-seq 'a 'b 'c))
      (g/merge-default-attrs 'a letter-attrs {:letter 'a})
      (g/merge-default-attrs 'b letter-attrs {:letter 'b})
      (g/merge-default-attrs 'c letter-attrs {:letter 'c})
      (g/set-attr 'a :self-support `(:permanent 1.1))
      ;(g/set-attr 'b :self-support `(:permanent 1.1))
      ;(g/set-attr 'c :self-support `(:permanent 1.1))
      ))

(defn start-model [fm]
  (with-state [fm fm]
    (update :stems assoc :bind 0 :support 0)
    (assoc :timestep 0
           :actions #{}
           :support-deltas {})  ; map {fromid {toid amt}}
    ))

;;; Bindings

(defn bdx? [fm id]
  (= :bind (class-of fm id)))

(defn all-bdx [fm]
  (->> fm g/edges (filter #(bdx? fm %))))

(defn add-bdx [fm from to]
  (g/add-edge fm [from :bdx-from] [to :bdx-to]
              {:class :bind
               :desiderata #{`(:need :mates-to-exist)
                             `(:oppose :other-bdx-from-same-mate)
                             `(:oppose :bindbacks)}
;               :desiderata #{:want-mates-to-exist
;                             :oppose-other-bdx-from-same-mate
;                             :oppose-bindbacks}
               :self-support `(:decaying 0.2)}))

(defn start-bdx-for
  "Creates bindings to id from all other nodes, and from id to all other
  nodes."
  [fm0 id]
  (with-state [fm fm0]
    (doseq [that-elem (all-nodes-other-than fm0 id)]
      (add-bdx id that-elem)
      (add-bdx that-elem id))))

(defn start-bdx-from-for
  "Creats bindings from id to all other nodes."
  [fm0 fromid]
  (with-state [fm fm0]
    (doseq [toid (all-nodes-other-than fm0 fromid)]
      (add-bdx fromid toid))))

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

(defn has-bdx-from?
  "Are any bindings, even if unofficial, linked to [id :bdx-from]?"
  [fm id]
  (seq (incident-bdx-from fm id)))

(defn bound-from
  "Returns id of element that bdxid is bound from."
  [fm bdxid]
  (->> (g/incident-ports fm bdxid)
       (filter #(= :bdx-from (second %)))
       ffirst))

(defn bound-to
  "Returns id of element that bdxid is bound to."
  [fm bdxid]
  (->> (g/incident-ports fm bdxid)
       (filter #(= :bdx-to (second %)))
       ffirst))

(defn bound-from? [fm fromid bdxid]
  (= fromid (bound-from fm bdxid)))

(defn bound-to? [fm toid bdxid]
  (= toid (bound-to fm bdxid)))

(defn bound-from-or-to? [fm id bdxid]
  (or (bound-from? fm id bdxid)
      (bound-to? fm id bdxid)))

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

;;; Support edges

(defn support-from
  "Returns id of supporting element."
  [fm suppid]
  (->> (g/incident-ports fm suppid)
       (filter #(= :support-to (second %)))
       ffirst))

(defn support-to
  "Returns id of supported element, i.e. the supportee."
  [fm suppid]
  (->> (g/incident-ports fm suppid)
       (filter #(= :support-from (second %)))
       ffirst))

;;; Printing

(defn ffmt [n]
  (format "%4.3f" n))

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
  attempted-weight, :weight weight}."
  [fm suppid]
  {:fromid (support-from fm suppid)
   :toid (support-to fm suppid)
   :attempted-weight (g/attr fm suppid :attempted-weight)
   :weight (g/weight fm suppid)})

(defn supportstr [fm suppid]
  (let [{:keys [fromid toid] :as m} (suppinfo fm suppid)]
    (format "%s  %-20s %s" suppid (eid fm suppid)
      (util/map-str (select-keys m [:attempted-weight :weight])))))

(defn support-totals-str [fm]
  (str 
    "(total :self-support " (ffmt (total-self-support fm)) ") "
    "(total :total-support " (ffmt (total-total-support fm)) ")"))

(defn pprint-support [fm]
  (cond
    :let [ids (all-support-edges fm)]
    (empty? ids)
      (println (str "Support: None  " (support-totals-str fm)))
    (let [total (total-of-all-pos-support-weights fm)]
      (println (str
        "Support:     (total pos :weight " (ffmt total) ") "
        (support-totals-str fm)))
      (doseq [s (->> ids (map #(supportstr fm %)) sort)]
        (println \space s)))))

(defn pprint-model [fm]
  (println "Gattrs:" (util/map-str (g/gattrs fm)))
  (g/pprint-nodes fm)
  ;TODO pprint-edges
  (pprint-bdx fm)
  (pprint-support fm))

(defn print-model-state [fm]
  #_(g/pprint fm)
  (pprint-model fm)
  fm)

;;; Actions

(defn unimplemented [& ignored]
  (throw (IllegalArgumentException. "Unimplemented function.")))

(def m-desideratum-objects
  {:bdx
    {:start-fn (fn [id] `(:start-bdx-for ~id))
     :find-all-fn incident-bdx
     :match?-fn bound-from-or-to?}
   :bdx-from
    {:start-fn (fn [id] `(:start-bdx-from-for ~id))
     :find-all-fn incident-bdx-from
     :match?-fn bound-from?}
   :adj-bdx
    {:start-fn unimplemented
     :find-all-fn (fn [fm id] (filter #(edge-to-adjacent? fm id %)
                                      (incident-bdx fm id)))
     :match?-fn (fn [fm id bdxid]
                  (and (bound-from-or-to? fm id bdxid)
                       (edge-to-adjacent? fm id bdxid)))}
   :mates-to-exist
    {:start-fn identity
     :find-all-fn (fn [fm bdxid] (g/incident-elems fm bdxid))
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
     :match?-fn unimplemented}})

(defn ospec-unsatisfied? [fm id ospec]
  (if (keyword? ospec)
    (empty? ((get-in m-desideratum-objects [ospec :find-all-fn]) fm id))
    false))

(defn desideratum->actions
  "Returns seq of actions, or nil if none."
  [fm id desideratum]
  (pmatch desideratum
    (:need ~@ospecs)
      (for [ospec ospecs
            :when (ospec-unsatisfied? fm id ospec)]
        ((get-in m-desideratum-objects [ospec :start-fn]) id))
    ~any
      nil))

(defn do-action [fm action]
  (pmatch action
    (:start-bdx-for ~id)
      (start-bdx-for fm id)
    (:start-bdx-from-for ~id)
      (start-bdx-from-for fm id)))

(defn do-actions [fm]
  (reduce #(do-action %1 %2) fm (:actions fm)))

(defn post-actions-for-desiderata [fm]
  (with-state [fm fm]
    (doseq [node (g/nodes fm)
            desideratum (g/attr fm node :desiderata)]
      (update :actions into (desideratum->actions fm node desideratum)))))

;;; Support for desiderata

(defn add-support
 ([fm fromid toid]
  (add-support fm fromid toid need-delta))
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
  (add-antipathy fm fromid toid antipathy-per-timestep))
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


(defn find-objects
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
        ~ls (guard (seq? ls))
          nil  ; ignore preferences
        ))))

(defn matching-objects
  [fm fromid subset object-spec]
  (let [match? (get-in m-desideratum-objects [object-spec :match?-fn])]
    (filter #(match? fm fromid %) subset)))

(defn preferred-objects
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

(defn post-support-for-desideratum
  "Adds/subtracts to :support-deltas for one desideratum."
  [fm fromid desideratum]
  (pmatch desideratum
    (:need ~@ospecs)
      (let [needed (find-objects fm fromid ospecs)
            preferred (preferred-objects fm fromid needed ospecs)]
        (with-state [fm fm]
          (doseq [toid needed]
            (add-support fromid toid))
          (doseq [toid preferred]
            (add-support fromid toid preference-delta))))
    (:oppose ~@ospecs)
      (let [fromid-prev (prev-total-support-for fm fromid)]
        (with-state [fm fm]
          (doseq [toid (find-objects fm fromid ospecs)]
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
        (bind amt (* positive-feedback-rate supportee-support))
        (when (not (close-to-zero? amt))
          (add-support fromid supportee amt))))))

;;; Updating support weights

(defn decay-self-support [fm elem]
  (pmatch (g/attr fm elem :self-support)
    (:decaying ~n)
      (g/set-attr fm elem :self-support `(:decaying ~(* 0.9 n)))
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
      (if (pos? prevts)
        (min prevts
             (+ (prev-weight fm edgeid)
                (* (delta fm fromid toid)
                   prevts)))
        0.0))))

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
        m (into {} (support-edges-and-attempted-weights fm))]
    (with-state [fm fm]
      (doseq [[suppid weight] (expt-scale-down-vals target-sum m)]
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
                   ;Is this right? Neg weights count negatively.
        m (into {} (support-edges-and-weights fm))]
    (with-state [fm fm]
      (doseq [[suppid weight] (expt-scale-down-vals support-limit m)]
        (g/set-attr suppid :weight weight)))))

(defn calculate-total-support [fm toid]
  (g/set-attr fm toid :total-support
    (+ (self-support-for fm toid)
       (reduce + (incoming-support-weights fm toid)))))

(defn calculate-total-supports [fm]
  (with-state [fm fm]
    (doseq [toid (elems-that-can-receive-support fm)]
      (calculate-total-support toid))
    normalize-total-supports))

;;; Removing graph elements

(defn remove-unsupported-elems [fm]
  (with-state [fm fm]
    (doseq [elem (elems-that-can-receive-support fm)]
      (when (and (< (self-support-for fm elem) minimum-self-support)
                 (< (total-support-for fm elem) minimum-total-support))
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

(defn do-timestep [fm]
  (with-state [fm fm]
    (update :timestep inc)
    -- (println "\n---------- timestep" (:timestep fm) "-----------\n")
    (assoc :actions #{}, :support-deltas {})

    save-prevs
    (doseq [elem (elems-that-can-receive-support fm)]
      (decay-self-support elem))
    remove-unsupported-elems
    post-actions-for-desiderata
    do-actions
    post-support-deltas-for-desiderata
    post-positive-feedback-for-support
    calculate-attempted-weights
    calculate-weights
    calculate-total-supports
    save-obs))

(defn demo
  "Run this to see what farg.model1 does."
  [& {:keys [log timesteps] :or {timesteps 3}}]
  (log/reset)
  (clear-data!)
  (with-logging log
    (with-state [fm (start-model abc)]
      calculate-total-supports
      save-obs
      print-model-state
      (dotimes [t timesteps]
        do-timestep
        ;-- (println)
        print-model-state
      )
      -- (write-data)))
  )
