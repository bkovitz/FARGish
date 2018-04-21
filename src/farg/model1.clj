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
            [farg.util :as util :refer [dd remove= find-first with-rng-seed]]
            [farg.with-state :refer [with-state]]))

; TODO
; demo
; start-model
; do-timestep
; pprint
; seek-desiderata  DONE
; start-bdx  DONE
; support-desiderata  DONE
; oppose-inconsistencies  
; post-support  DONE
; post-antipathy
; normalize-support
; update-total-support

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

(defn total-of-all-support-weights [fm]
  (->> (all-support-edges fm)
       (map #(g/weight fm %))
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
   :desiderata #{:want-bdx :want-adj-bdx}})
;TODO :want-bdx-from, not :want-bdx

(def abc
  (-> (graph (left-to-right-seq 'a 'b 'c))
      (g/merge-default-attrs 'a letter-attrs {:letter 'a})
      (g/merge-default-attrs 'b letter-attrs {:letter 'b})
      (g/merge-default-attrs 'c letter-attrs {:letter 'c})
      ))

(defn start-model [fm]
  (with-state [fm fm]
    (update :stems assoc :bind 0 :support 0)
    (assoc :timestep 0
           :actions #{}
           :support-deltas [])
    ))

;;; Bindings

(defn bdx? [fm id]
  (= :bind (class-of fm id)))

(defn all-bdx [fm]
  (->> fm g/edges (filter #(bdx? fm %))))

(defn add-bdx [fm from to]
  (g/add-edge fm [from :bdx-from] [to :bdx-to]
              {:class :bind
               :desiderata #{:want-mates-to-exist}
               :self-support `(:decaying 0.2)}))

(defn start-bdx-for
  "Creates bindings to id from all other nodes, and from id to all other
  nodes."
  [fm0 id]
  (with-state [fm fm0]
    (doseq [that-elem (all-nodes-other-than fm0 id)]
      (add-bdx id that-elem)
      (add-bdx that-elem id))))

(defn has-bdx?
  "Are any bindings, even if unofficial, linked to [id :bdx-from] or
  [id :bdx-to]?"
  [fm id]
  (or (seq (g/port->incident-edges fm [id :bdx-from]))
      (seq (g/port->incident-edges fm [id :bdx-to]))))

(defn incident-bdx [fm id]
  (concat
    (g/port->incident-edges fm [id :bdx-from])
    (g/port->incident-edges fm [id :bdx-to])))

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

(defn supportstr [fm suppid]
  (let [[fromid toid] (->> suppid
                           (g/incident-ports fm)
                           (sort-by second)
                           (map first))]
    (str suppid "  " fromid " -> " toid "  " (g/weight fm suppid))))

(defn pprint-support [fm]
  (cond
    :let [ids (all-support-edges fm)
          totaltotal (total-total-support fm)]
    (empty? ids)
      (println (str "Support: None  (totaltotal of :total-supports "
                    totaltotal ")"))
    (let [total (total-of-all-support-weights fm)]
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
    :want-bdx (when (not (has-bdx? fm id))
                [`(:start-bdx-for ~id)])
    nil))

(defn do-action [fm action]
  (pmatch action
    (:start-bdx-for ~id)
      (start-bdx-for fm id)))

(defn do-actions [fm]
  (reduce #(do-action %1 %2) fm (:actions fm)))

(defn post-actions-for-desiderata [fm]
  (with-state [fm fm]
    (doseq [node (g/nodes fm)
            desideratum (g/attr fm node :desiderata)]
      (update :actions into (desideratum->actions fm node desideratum)))))

;;; Support for desiderata, second attempt

(defn desideratum->support-deltas
  "Returns a seq of deltas for :support, or nil if none."
  [fm fromid desideratum]
  (case desideratum
    :want-mates-to-exist  ; assumes that if is an edge
      (for [toid (g/incident-elems fm fromid)]
        `(:add-support ~fromid ~toid ~desideratum-delta-per-timestep))
    :want-bdx
      (for [toid (incident-bdx fm fromid)]
        `(:add-support ~fromid ~toid ~desideratum-delta-per-timestep))
    :want-adj-bdx
      (for [toid (->> (incident-bdx fm fromid)
                      (filter #(edge-to-adjacent? fm fromid %)))]
        `(:add-support ~fromid ~toid ~desideratum-delta-per-timestep))))

(defn post-support-deltas-for-desiderata [fm]
  (with-state [fm fm]
    (doseq [id (elems-that-can-give-support fm)
            desideratum (g/attr fm id :desiderata)]
      (update :support-deltas
        into (desideratum->support-deltas fm id desideratum)))))

(defn post-positive-feedback-for-support [fm]
  (with-state [fm fm]
    (doseq [fromid (elems-that-can-give-support fm)
            supportee (supportees-of fm fromid)]
      (bind amt (* positive-feedback-rate (total-support-for fm supportee)))
      (when (not (close-to-zero? amt))
        (update :support-deltas
          conj `(:add-support ~fromid ~supportee ~amt))))))

;;; Updating support weights, second attempt

(defn normalize-outgoing-support [fm fromid]
  (let [support-edges (outgoing-support-edges fm fromid)
        old-weights (into {} (map #(vector % (g/weight fm %)) support-edges))
        target-sum (util/clamp [0.2 1.0] (reduce + (vals old-weights)))
        new-weights (util/normalize-vals target-sum old-weights)]
    (with-state [fm fm]
      (doseq [[edgeid new-weight] new-weights]
        (g/set-attr edgeid :weight new-weight)))))

(defn apply-support-delta [fm delta]
  (pmatch delta
    (:add-support ~fromid ~toid ~amt)
      (cond
        :let [edgeid (find-support-edge fm fromid toid)]
        (nil? edgeid)
          (add-support-edge fm fromid toid amt)
        (g/add-weight fm edgeid amt))))

(defn apply-support-deltas [fm]
  (with-state [fm fm]
    (doseq [delta (:support-deltas fm)]
      (apply-support-delta delta))
    -- (logdo :support
         (println "\nunnormalized support:\n")
         (pprint-support fm))
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
  (let [ptotal (total-permanent-support fm)
        m (->> (elems-that-can-receive-support fm)
               (map #(vector % (total-support-for fm %)))
               (into {}))
        target-sum (util/clamp
                     [ptotal (+ ptotal (total-of-all-support-weights fm))]
                     (total-total-support fm))
        new-m (util/normalize-vals target-sum m)]
    (dd ptotal m target-sum new-m)
    (with-state [fm fm]
      (doseq [[id new-total-support] new-m]
        (g/set-attr id :total-support new-total-support)))))

(defn update-total-support [fm0]
  (with-state [fm fm0]
    ;self-support
    (doseq [elem (elems-that-can-receive-support fm0)]
      (set-total-support-to-self-support elem))
    ;support received through :support edges
    (doseq [[fromid m] (support-map fm0)
            [toid weight] m]
      (bind from-support (total-support-for fm0 fromid))
      (g/update-attr toid :total-support
        + (* weight from-support #_(min 1.0 from-support))))
    normalize-total-support
    ))

;;; Running

(defn do-timestep [fm]
  (with-state [fm fm]
    (update :timestep inc)
    (assoc :actions #{}, :support-deltas [])

    post-actions-for-desiderata
    do-actions
    post-support-deltas-for-desiderata
    post-positive-feedback-for-support
    apply-support-deltas
    update-total-support
    ; TODO: rm unsupported elems
  ))

(defn demo
  "Run this to see what farg.model1 does."
  [& {:keys [logk timesteps] :or {timesteps 3}}]
  ;[& logk]
  (with-logging logk
    (with-state [fm (start-model abc)]
      update-total-support
      print-model-state
      (dotimes [t timesteps]
        do-timestep
        -- (println)
        print-model-state
      ))))
