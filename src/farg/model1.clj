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
; demo  DONE
; start-model  DONE
; do-timestep  DONE
; pprint  DONE
; seek-desiderata  DONE
; start-bdx  DONE
; support-desiderata  DONE
; oppose-inconsistencies  
; post-support  DONE
; post-antipathy
; normalize-support  DONE
; update-total-support  DONE

;(def default-node-attrs
;  {:total-support 1.0
;   :self-support '(permanent 1.0)

;(defn add-desiderata [fm]
;  (with-state [fm fm]
;    (doseq [node (g/nodes fm)]
;      (when (= :letter (attr fm node :class))
;        (set-attr node :desiderata 

;;; Global parameters (these belong in the FARG model itself)

(def desideratum-delta-per-timestep 0.1)
(def positive-feedback-rate 0.1)
(def normalization-expt 1.2)
  ; Applied when normalizing a set of values whose sum exceeds some limit.
  ; Must be > 1.0 for higher values to drive down lower values.

(defn raise-to-norm-expt [n]
  (cond
    (> n 0.0)
      (Math/pow n normalization-expt)
    (< n 0.0)
      (- (Math/pow (- n) normalization-expt))
    0.0))

(defn normalize-vals [target-sum m]
  (util/normalize-vals target-sum (mapvals raise-to-norm-expt m)))

;;; Utility functions

(defn close-to-zero? [x]
  (< (Math/abs x) 0.001))

(defn all-nodes-other-than [fm id]
  (->> (g/nodes fm) (remove= id)))

(defn class-of [fm id]
  (g/attr fm id :class))

;;; Utility functions for support

(defn support? [fm id]
  (= :support (class-of fm id)))

(defn all-support-edges [fm]
  (filter #(support? fm %) (g/edges fm)))

(defn elems-that-can-give-support [fm]
  (filter #(not (support? fm %)) (g/elems fm)))

(def elems-that-can-receive-support elems-that-can-give-support)

(defn find-support-edge [fm fromid toid]
  (g/find-edgeid fm [fromid :support-from] [toid :support-to]))

(defn remove-support-edge [fm fromid toid]
  (g/remove-edge fm [fromid :support-from] [toid :support-to]))

(defn add-support-edge [fm fromid toid weight]
  (if (close-to-zero? weight)
    (remove-support-edge fm fromid toid)
    (g/add-edge fm [fromid :support-from] [toid :support-to]
                   {:class :support, :weight weight})))

(defn outgoing-support-edges [fm fromid]
  (g/port->incident-edges fm [fromid :support-from]))

(defn supportees-of
  "Elements (both nodes and edges) to which there is a support edge from
  fromid. Note that a supportee might be getting negative support."
  [fm fromid]
  (->> (outgoing-support-edges fm fromid)
       (map #(g/other-id fm fromid %))))

(defn total-support-for [fm id]
  (cond
    :let [s (g/attr fm id :total-support)]
    (nil? s) 0.0
    s))

(defn support-map
  "Returns map {fromid {toid weight}} of all existing support in fm."
  [fm]
  (with-state [m {}]
    (doseq [edgeid (all-support-edges fm)]
      (bind emap (g/edge-as-map fm edgeid))
      (assoc-in [(:support-from emap) (:support-to emap)] (:weight emap)))))

(defn total-of-all-pos-support-weights [fm]
  (->> (all-support-edges fm)
       (map #(g/weight fm %))
       (filter #(> % 0.0))
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
       (reduce +)))

;;; Initialization

(def letter-attrs
  {:class :letter
   :self-support `(:permanent 1.0)
   :desiderata #{:want-bdx-from :want-adj-bdx}})

(def abc
  (-> (graph (left-to-right-seq 'a 'b 'c))
      (g/merge-default-attrs 'a letter-attrs {:letter 'a})
      (g/merge-default-attrs 'b letter-attrs {:letter 'b})
      (g/merge-default-attrs 'c letter-attrs {:letter 'c})
      (g/set-attr 'a :self-support `(:permanent 1.1))
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
               :desiderata #{:want-mates-to-exist
                             :oppose-other-bdx-from-same-mate}
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

(defn incident-bdx [fm id]
  (concat
    (g/port->incident-edges fm [id :bdx-from])
    (g/port->incident-edges fm [id :bdx-to])))

(defn incident-bdx-from [fm id]
  (g/port->incident-edges fm [id :bdx-from]))

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

(defn edge-to-adjacent? [fm id edgeid]
  (let [mate (g/other-id fm id edgeid)]
    (or (= mate (g/attr fm id :adj-right))
        (= mate (g/attr fm id :adj-left)))))

;;; Printing

(defn bdxstr [fm bdxid]
  (let [[fromid toid] (->> bdxid
                           (g/incident-ports fm)
                           (sort-by second)
                           (map first))]
    (str bdxid \space fromid " -> " toid (g/attrstr fm bdxid))))

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

(defn supportstr [fm suppid]
  (let [[fromid toid] (->> suppid
                           (g/incident-ports fm)
                           (sort-by second)
                           (map first))
        weight (g/weight fm suppid)
        ts (total-support-for fm fromid)]
    (str suppid "  " fromid " -> " toid "  " weight
         "  (actual: " (* weight (max ts 0.0)) ")")))

(defn pprint-support [fm]
  (cond
    :let [ids (all-support-edges fm)
          totaltotal (total-total-support fm)]
    (empty? ids)
      (println (str "Support: None  (totaltotal of :total-supports "
                    totaltotal ")"))
    (let [total (total-of-all-pos-support-weights fm)]
      (println (str
        "Support:     (total " total ") (total of :total-supports "
        totaltotal ")"))
      (doseq [s (->> ids (map #(supportstr fm %)) sort)]
        (println \space s)))))

(defn pprint-model [fm]
  (println "Gattrs:" (g/gattrs fm))
  (g/pprint-nodes fm)
  ;TODO pprint-edges
  (pprint-bdx fm)
  (pprint-support fm))

(defn print-model-state [fm]
  #_(g/pprint fm)
  (pprint-model fm)
  fm)

;;; Actions

(defn desideratum->actions
  "Returns seq of actions, or nil if none."
  [fm id desideratum]
  (case desideratum
    :want-bdx
      (when (not (has-bdx? fm id))
        [`(:start-bdx-for ~id)])
    :want-bdx-from
      (when (not (has-bdx-from? fm id))
        [`(:start-bdx-from-for ~id)])
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
  (add-support fm fromid toid desideratum-delta-per-timestep))
 ([fm fromid toid amt]
  (update-in fm [:support-deltas fromid toid]
    (fn [old-delta]
      (cond
        (nil? old-delta)
          amt
        (< old-delta 0.0)
          old-delta  ; don't add support if fromid has antipathy to toid
        (+ old-delta amt))))))

(defn add-antipathy [fm fromid toid]
  (update-in fm [:support-deltas fromid toid]
    (fn [old-delta]
      (cond
        (or (nil? old-delta)
            (>= old-delta 0.0))
          (- desideratum-delta-per-timestep) ;replace support with antipathy
        ;add to existing antipathy
        (- old-delta desideratum-delta-per-timestep)))))

(defn post-support-for-desideratum
  "Adds/subtracts to :support-deltas for one desideratum."
  [fm fromid desideratum]
  (with-state [fm fm]
    (case desideratum
      :want-mates-to-exist  ; assumes that fromid is an edge
        (doseq [toid (g/incident-elems fm fromid)]
          (add-support fromid toid))
      :want-bdx
        (doseq [toid (incident-bdx fm fromid)]
          (add-support fromid toid))
      :want-bdx-from
        (doseq [toid (incident-bdx-from fm fromid)]
          (add-support fromid toid))
      :want-adj-bdx
        (doseq [toid (->> (incident-bdx fm fromid)
                        (filter #(edge-to-adjacent? fm fromid %)))]
          (add-support fromid toid))
      :oppose-other-bdx-from-same-mate
        (doseq [toid (->> (bound-from fm fromid)
                          (incident-bdx-from fm)
                          (remove= fromid))]
          (add-antipathy fromid toid))
      ;NEXT: :oppose-tight-bdx-cycle
        )))

(defn post-support-deltas-for-desiderata [fm]
  (with-state [fm fm]
    (doseq [id (elems-that-can-give-support fm)
            desideratum (g/attr fm id :desiderata)]
      (post-support-for-desideratum id desideratum))))

(defn post-positive-feedback-for-support [fm]
  (with-state [fm fm]
    (doseq [fromid (elems-that-can-give-support fm)
            supportee (supportees-of fm fromid)]
      (bind amt (* positive-feedback-rate (total-support-for fm supportee)))
      (when (not (close-to-zero? amt))
        (add-support fromid supportee amt)))))

;;; Updating support weights

(defn normalize-outgoing-support [fm fromid]
  (let [support-edges (outgoing-support-edges fm fromid)
        old-pos-weights (->> support-edges
                             (map #(vector % (g/weight fm %)))
                             (filter #(> (second %) 0.0))
                             (into {}))
        old-pos-sum (reduce + (vals old-pos-weights))
        old-neg-weights (->> support-edges
                             (map #(vector % (g/weight fm %)))
                             (filter #(< (second %) 0.0))
                             (into {}))
        old-neg-sum (reduce + (vals old-neg-weights))]
    (with-state [fm fm]
      (when (> old-pos-sum 1.0)
        (doseq [[edgeid new-weight] (normalize-vals 1.0 old-pos-weights)]
          (g/set-attr edgeid :weight new-weight)))
      (when (< old-neg-sum -1.0)
        (doseq [[edgeid new-weight] (normalize-vals -1.0 old-neg-weights)]
          (g/set-attr edgeid :weight new-weight))))))

(defn apply-support-deltas [fm]
  (with-state [fm fm]
    (doseq [[fromid m-deltas] (:support-deltas fm)
            [toid amt] m-deltas]
      (bind edgeid (find-support-edge fm fromid toid))
      (if (nil? edgeid)
        (add-support-edge fromid toid amt)
        (g/add-weight edgeid amt)))
    (doseq [fromid (elems-that-can-give-support fm)]
      (normalize-outgoing-support fromid))))

;;; Total support

(defn set-total-support-to-self-support [fm elem]
  (pmatch (g/attr fm elem :self-support)
    (:permanent ~n)
      (g/set-attr fm elem :total-support n)
    (:decaying ~n)
      (-> fm
          (g/set-attr elem :total-support n)
          (g/set-attr elem :self-support `(:decaying ~(* 0.9 n))))
    nil
      (g/set-attr fm elem :total-support 0.0)))

(defn normalize-total-support [fm]
  (let [support-limit (+ (total-permanent-support fm)
                         (total-of-all-pos-support-weights fm))
        actual-total-support (total-total-support fm)]
    (with-state [fm fm]
      (when (> actual-total-support support-limit)
        (bind m (->> (elems-that-can-receive-support fm)
                  (map #(vector % (total-support-for fm %)))
                  (into {})))
        (doseq [[id new-total-support] (normalize-vals support-limit m)]
          (g/set-attr id :total-support new-total-support))))))

(defn update-total-support [fm0]
  (with-state [fm fm0]
    ;self-support
    (doseq [elem (elems-that-can-receive-support fm0)]
      (set-total-support-to-self-support elem))
    ;support received through :support edges
    (doseq [[fromid m] (support-map fm0)
            [toid weight] m]
      (bind from-support (total-support-for fm0 fromid))
      (when (and (> from-support 0.0))
        (g/update-attr toid :total-support
          + (* weight from-support))))
    normalize-total-support
    ))

;;; Running

(defn do-timestep [fm]
  (with-state [fm fm]
    (update :timestep inc)
    -- (println "\n---------- timestep" (:timestep fm) "-----------\n")
    (assoc :actions #{}, :support-deltas {})

    post-actions-for-desiderata
    do-actions
    post-support-deltas-for-desiderata
    -- (log :support (:support-deltas fm))
    post-positive-feedback-for-support
    apply-support-deltas
    update-total-support
    ; TODO: rm unsupported elems
  ))

(defn demo
  "Run this to see what farg.model1 does."
  [& {:keys [log timesteps] :or {timesteps 3}}]
  (log/reset)
  (with-logging log
    (with-state [fm (start-model abc)]
      update-total-support
      print-model-state
      (dotimes [t timesteps]
        do-timestep
        -- (println)
        print-model-state
      ))))
