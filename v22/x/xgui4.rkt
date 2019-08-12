; Throwaway code to figure out how to make a pict respond to a mouseover

#lang racket/gui
(require racket/class
         racket/snip
         racket/format
         pict
         pict/snip)

(define frame
  (new frame%
       [label "Example"]
       [width 300]
       [height 300]))

(define msg
  (new message%
       [parent frame]
       [label "No events so far..."]))

(define ec (new editor-canvas%
                [parent frame]))

(define pb (new pasteboard%))
(send ec set-editor pb)

(define my-canvas%  ; This didn't turn out to be needed for the mouseover
  (class canvas%
    (define/override (on-event event)
      (send msg set-label "Canvas mouse"))
    (define/override (on-char event)
      (send msg set-label "Canvas keyboard"))
    (super-new)))

(define hover-pict-snip%
  (class pict-snip%
    (define/override (on-event dc x y editorx editory event)
      (send msg set-label "Hover!"))
    (super-new)))

;(define p (disk 20 #:color "teal"))
(define p (rotate (colorize (hline 100 20) "lavender") (/ pi 4)))

(define ps (new hover-pict-snip% [pict p]))
(send ps set-flags '(handles-all-mouse-events))

(send pb insert ps)

(send frame show #t)

;; Experiments with elements of the spreading-activation display

;(define (mkdisk)
;  (disk 10.0 #:draw-border? #f #:color "Aquamarine"))
;
;(define sa-snip%
;  (class pict-snip%
;    (define/override (draw dc x y left top right bottom dx dy draw-caret)
;      (
