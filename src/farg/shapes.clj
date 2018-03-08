(ns farg.shapes
  "Functions and data to convert FARG nodes, edges, text, etc. into
  seesaw shapes."
  (:refer-clojure :exclude [cond rand rand-int])
  (:require [better-cond.core :refer [cond]]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [com.rpl.specter :as S]
            [farg.pgraph :as g :refer [pgraph]]
            [farg.no-reload :refer [gui-state]]
            [farg.util :as util :refer [dd dde]]
            [farg.with-state :refer [with-state]]
            [seesaw.core :as seesaw
              :refer [native! frame pack! show! config config! label alert
                      border-panel scrollable canvas listen select dispose!
                      menu menubar label table xyz-panel top-bottom-split
                      to-root repaint!]]
            [seesaw.graphics
              :refer [draw rotate rect ellipse style stroke string-shape
                      anti-alias]]
            [seesaw.action :refer [action]]
            [seesaw.keymap :refer [map-key]]
            [seesaw.color :refer [color]]
            [seesaw.font :refer [font]]
            [seesaw.table :as table]))

(def text-font (font :name "TIMES" :size 20))
(def text-style (style :foreground (color 0 0 0)
                       :font #_"TIMES-BOLD-24" text-font))

(def rect-style (style :foreground :black
                       :background :white
                       :stroke (stroke :width 1)))

(let [bi (java.awt.image.BufferedImage. 200 100
           (. java.awt.image.BufferedImage TYPE_INT_ARGB))
      g2d (. bi getGraphics)
      frc (. g2d getFontRenderContext)]
  (defn calc-string-bounds [font- s]
    (. font- getStringBounds s frc)))

(defn x-position-within-parent [x-positioning parent-box relative-box]
  (case x-positioning
    :centered
      (- (. parent-box getCenterX) (. relative-box getCenterX))))

(defn y-position-within-parent [y-positioning parent-box relative-box]
  (case y-positioning
    :top
      (- (. parent-box getY) (. relative-box getCenterY))
    :just-below-top
      (+ (. parent-box getY)
         (- (. relative-box getY))
         2)
    :centered
      (- (. parent-box getCenterY) (. relative-box getCenterY))
    :just-above-bottom
      (+ (. parent-box getY)
         (. parent-box getHeight)
         (. relative-box getY)
         2)
    :bottom
      (+ (. parent-box getY)
         (. parent-box getHeight)
         (- (. relative-box getCenterY)))))

(defn position-within-parent
  [x-positioning y-positioning parent-box relative-box]
  [(x-position-within-parent x-positioning parent-box relative-box)
   (y-position-within-parent y-positioning parent-box relative-box)])

(def min-text-rect-width-height 30)

(defn text-centered-in-rect [s [x y]]
  (let [relative-box (calc-string-bounds text-font s)
        w (max (+ 10 (. relative-box getWidth)) min-text-rect-width-height)
        h (max (. relative-box getHeight) min-text-rect-width-height)
        rel-y (. relative-box getCenterY)
        rectangle (rect (- x (/ w 2.0)) (- y rel-y) w h)
        [textx texty] (position-within-parent :centered :centered
                                              rectangle relative-box)]
    [rectangle
     rect-style
     (string-shape textx texty s)
     text-style]))

(defn farg-shape->seesaw
  "Returns a seq containing one or more seesaw shapes each followed by a
  seesaw style, suitable for passing as consecutive arguments to
  seesaw.graphics/draw, as happens in 'render'."
  [{:keys [type] :as shape}]
  (case type
    ::text
      (let [[x y] (:xy shape)]
        [(string-shape x y (:text shape)) text-style])
    ::node
      (text-centered-in-rect (:text shape) (:xy shape))
  ))

(defn text [s xy]
  {:type ::text, :text s, :xy xy})

(defn node [s xy]
  {:type ::node, :text s, :xy xy})
