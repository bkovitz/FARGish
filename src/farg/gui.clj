(ns farg.gui
  (:refer-clojure :exclude [cond rand rand-int])
  (:require [better-cond.core :refer [cond]]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [com.rpl.specter :as S]
            [farg.graphs :as g :refer [graph]]
            [farg.no-reload :refer [gui-state]]
            [farg.shapes :as shapes :refer [farg-shape->seesaw]]
            [farg.util :as util :refer [dd find-first]]
            [farg.with-state :refer [with-state]]
            [seesaw.behave :as behave :refer [when-mouse-dragged]]
            [seesaw.core :as seesaw
              :refer [native! frame pack! show! config config! label alert
                      border-panel scrollable canvas listen select dispose!
                      menu menubar label table xyz-panel top-bottom-split
                      to-root repaint!]]
            [seesaw.graphics :refer [draw rotate anti-alias]]
            [seesaw.action :refer [action]]
            [seesaw.keymap :refer [map-key]]
            [seesaw.color :refer [color]]
            [seesaw.font :refer [font]]
            [seesaw.table :as table]))

;NEXT
; Scale/invert coordinates from FARGish to Seesaw.  DONE
; Draw edges.

(native!)

(def a (atom nil))

(defn render
  "Renders the canvas that shows the graph of the FARG model. All :shapes
  in @gui-state must be prepared before 'render' is called. 'render' only
  draws the :seesaw element of each shape."
  [c g2d]
  (anti-alias g2d)
  (let [seesaw-shapes (mapcat :seesaw (:shapes @gui-state))]
    (apply draw g2d seesaw-shapes)))

(defn update-shapes!
  "Updates the :shapes in @gui-state to match the FARG model in :fm, including
  giving each shape its :seesaw element so that 'render' can draw them all."
  []
  (swap! gui-state
    (fn [{:keys [fm] :as gui-state}]
      (assoc gui-state :shapes
        (->> fm shapes/g->farg-shapes (map shapes/add-seesaw))))))

(defn update!
  "Call this to force the GUI to redraw."
  [& _]
  (update-shapes!)
  (repaint! (:frame @gui-state)))

(defn stub-shapes!
  "Sticks some hard-coded shapes into the GUI window. Useful for testing to see
  how things look."
  []
  (do
    (swap! gui-state update :shapes (fn [old-shapes]
      #{(shapes/node "abcdefg" [50 100])
        (shapes/text "Salve, Munde!" [200 300])}))
    nil))

(def stub-model
  (with-state [g (graph (left-to-right-seq 'a 'b))]
    (g/add-tag :succ 'a 'b)))

(defn stub-model! []
  (swap! gui-state assoc :fm stub-model))


(defn point-in-shape? [point seesaw-shape]
  (let [x (.x point), y (.y point)]
    (and (isa? (type seesaw-shape) java.awt.Shape)
         (.contains seesaw-shape x y))))

(defn find-seesaw-shape [point]
  (find-first #(point-in-shape? point %) (:shapes @gui-state)))

(defn start [event & args]
  )
;  (dd "start" event (.getPoint event) args
;      (find-seesaw-shape (.getPoint event))))

(defn drag [event & args]
  )
;  (dd "drag" event (.getPoint event) args
;      (find-seesaw-shape (.getPoint event))))

(defn finish [event & args]
  )
;  (dd "finish" event (.getPoint event) args
;      (find-seesaw-shape (.getPoint event))))

(defn add-behaviors [root]
  (let [canvas (select root [:#canvas])]
    (when-mouse-dragged canvas
      :start start
      :drag drag
      :finish finish))
  root)

(defn make-gui-frame
  "Returns the Seesaw frame for the whole GUI."
  []
  (frame
    :title "FARGish"
    :size [800 :by 800]
    :content (scrollable (canvas :id :canvas
                                 :paint render
                                 ;:size [2000 :by 1200]
                                 :background :white))))

(defn make-gui
  "Sets up the whole GUI and returns the main frame."
  []
  (-> (make-gui-frame) add-behaviors))

(defn run
  "Run this to start the GUI. Put FARG shapes (see farg.shapes) into
  (:shapes @gui-state) to queue them up for drawing. Call update! to
  repaint the window."
  []
  (swap! gui-state update :frame (fn [old-frame]
    (when (some? old-frame)
      (dispose! old-frame))
    (-> (make-gui) show!))))

(defn restart!
  "For debugging the GUI code."
  []
  (reset! gui-state farg.no-reload/empty-gui-state)
  (stub-model!)
  (update-shapes!)
  (run))
