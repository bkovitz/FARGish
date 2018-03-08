(ns farg.gui
  (:refer-clojure :exclude [cond rand rand-int])
  (:require [better-cond.core :refer [cond]]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [com.rpl.specter :as S]
            [farg.pgraph :as g :refer [pgraph]]
            [farg.no-reload :refer [gui-state]]
            [farg.shapes :as shapes :refer [farg-shape->seesaw]]
            [farg.util :as util :refer [dd dde]]
            [farg.with-state :refer [with-state]]
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

(native!)

(defn render [c g2d]
  (anti-alias g2d)
  (->> (:shapes @gui-state) (mapcat farg-shape->seesaw) (apply draw g2d)))

(defn update!
  "Call this to force the GUI to redraw."
  [& _]
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

(defn make-gui
  "Returns the Seesaw frame for the whole GUI."
  []
  (frame
    :title "FARGish"
    :size [800 :by 800]
    :content (scrollable (canvas :id :canvas
                                 :paint render
                                 ;:size [2000 :by 1200]
                                 :background :white))))

(defn run
  "Run this to start the GUI. Put FARG shapes (see farg.shapes) into
  (:shapes @gui-state) to queue them up for drawing. Call update! to
  repaint the window."
  []
  (swap! gui-state update :frame (fn [old-frame]
    (when (some? old-frame)
      (dispose! old-frame))
    (-> (make-gui) show!))))
