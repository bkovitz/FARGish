(ns farg.logging
  "Simple, flexible logging.

  Naming convention: a logk is a \"log key\": a keyword or seq of keywords
  that names the logging stream(s) that a message should go to.

  with-log establishes which streams to log.

  The log* macros send messages to the log and indicate which stream the
  messages are part of."
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [farg.util :as util :refer [dd remove= find-first with-rng-seed]]))

(def default-logging #{:error})

(def logging "Atom containing set of keywords indicating which actions to log."
  (atom default-logging))

(defn reset []
  (reset! logging default-logging))

(def log-writer "Atom containing java.io.Writer to write log messages to."
  (atom *err*))

(defn bad-logk [funcname logk]
  (throw (IllegalArgumentException. (str funcname ": "
    (pr-str logk) " has unexpected type: " (type logk)))))

(defn print-to-log [& xs]
  (let [w @log-writer]
    (doseq [x xs]
      (.write w (str x)))))

(defn println-to-log [& xs]
  (apply print-to-log xs)
  (let [w @log-writer]
    (.write w "\n")
    (.flush w)))

(defn pprint-to-log [& xs]
  (let [w @log-writer]
    (doseq [x xs]
      (pprint x w))
    (.flush w)))

(defn log-this? [logk]
  (cond
    (keyword? logk)
      (contains? @logging logk)
    (seqable? logk)
      (some log-this? logk)
    (bad-logk "log-this?" logk)))

(defn start-logging [logk]
  (cond
    (keyword? logk)
      (swap! logging conj logk)
    (seqable? logk)
      (swap! logging into logk)
    (bad-logk "start-logging" logk)))

(defn stop-logging [logk]
  (cond
    (keyword? logk)
      (swap! logging disj logk)
    (seqable? logk)
      (swap! logging #(apply disj % logk))
    (bad-logk "stop-logging" logk)))

(defmacro logdo
  "Inside a logdo, *out* is bound to the log file, so logdo is useful to
  include a sequence of one or more print or pprint statements that output
  log data."
  [logk & body]
  `(when (log-this? ~logk)
     (util/with-*out* @log-writer
       ~@body)))

(defmacro log [logk & exprs]
  `(when (log-this? ~logk)
     (println-to-log ~@exprs)))

(defmacro log-pprint [logk & exprs]
  `(when (log-this? ~logk)
     (pprint-to-log ~@exprs)))

(defn logdd- [exprs]
  (cons 'do
        (for [expr exprs]
          `(let [v# ~expr
                 v# (if (seq? v#) (doall v#) v#)]
             (print-to-log '~expr " => ")
             (pprint-to-log v#)
             v#))))

;TODO Throw error if exprs is empty
;TODO UT that this really returns the value of the last expr
(defmacro logdd [logk & exprs]
  `(if (log-this? ~logk)
     ~(logdd- exprs)
     (do ~@exprs)))

(defmacro with-logging [logk & body]
  `(let [save# @logging]
     (start-logging ~logk)
     (let [result# (do ~@body)]
       (reset! logging save#)
       result#)))
