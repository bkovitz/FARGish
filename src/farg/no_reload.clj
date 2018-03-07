(ns farg.no-reload
  "Variables not to trash when reloading or refreshing in the REPL.")

(clojure.tools.namespace.repl/disable-reload! *ns*)

(def gui-state (atom
  {:frame nil ;The JFrame that holds the whole GUI
   :shapes #{}})) ;gui/render passes each elem through farg-shape->seesaw and
                  ;then to seesaw.graphics/draw.

