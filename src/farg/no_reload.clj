(ns farg.no-reload
  "Variables not to trash when reloading or refreshing in the REPL.")

(clojure.tools.namespace.repl/disable-reload! *ns*)

(def empty-gui-state
  {:frame nil    ;The JFrame that holds the whole GUI
   :fm nil       ;The current FARG model
   :shapes #{}})

(def gui-state (atom empty-gui-state))

;gui/render passes each elem through farg-shape->seesaw and
                  ;then to seesaw.graphics/draw.

