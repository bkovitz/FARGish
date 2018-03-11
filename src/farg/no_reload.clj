(ns farg.no-reload
  "Variables not to trash when reloading or refreshing in the REPL.")

(clojure.tools.namespace.repl/disable-reload! *ns*)

(def empty-gui-state ^{:type ::gui-state}
  {:frame nil             ;The JFrame that holds the whole GUI
   :fm nil                ;The current FARG model
   :shapes #{}}           ;Set of farg-shapes corresponding to :fm
   :drag-in-progress nil) ;A drag-in-progress, if any

(def gui-state (atom empty-gui-state))

(defmethod print-method ::gui-state [v ^java.io.Writer w]
  (.write w "#gui-state"))
