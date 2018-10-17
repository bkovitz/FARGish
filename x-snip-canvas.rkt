; Throwaway code to figure out mrlib/snip-canvas

#lang debug at-exp racket/gui
(require racket/class
         racket/snip
         racket/format
         pict
         pict/snip
         mrlib/snip-canvas)

(define fr
  (new frame%
       [label "Example"]
       [width 300]
       [height 300]))

(define p (disk 20 #:color "teal"))
(define p2 (disk 20 #:color "aquamarine"))

(define hover-pict-snip%
  (class pict-snip%
    (define/override (on-event dc x y editorx editory event)
      (displayln "Event!"))
    (super-new)
    (send this set-flags '(handles-all-mouse-events))))

(define (labeled-disk name diameter)
  (inset (vc-append (inset (text name) 0 2)
                    (cc-superimpose
                      (ghost (rectangle 20 20))
                      (disk diameter #:draw-border? #f #:color "Aquamarine")))
         4.0 8.0))

(define labeled-disk%
  (class pict-snip%
    (init-field name diameter)
    (super-new [pict (labeled-disk name diameter)])
    (define/override (on-event dc x y editorx editory event)
      (when (eq? 'left-down (send event get-event-type))
        (displayln (list x y editorx editory
                         (send event get-x) (send event get-y)))))
    (send this set-flags '(handles-all-mouse-events))))

;  (class snip-canvas%
;    (init-field name diameter)
;    (define (
;    (super-new [make-snip (λ (w h)


;(define pane (new horizontal-pane% [parent fr]))
;
;(define sc (new snip-canvas%
;                [parent pane]
;                [make-snip (λ _ #R _ (new hover-pict-snip% [pict p]))]))
;(define sc2 (new snip-canvas%
;                [parent pane]
;                [make-snip (λ _ #R _ (new hover-pict-snip% [pict p2]))]))

(define ld1 (new labeled-disk% [name "this"] [diameter 9.2]))
(define sc1 (new snip-canvas%
                 [parent fr]
                 [make-snip (λ _ ld1)]))

(define ld2 (new labeled-disk% [name "second one"] [diameter 13.2]))
(define sc2 (new snip-canvas%
                 [parent fr]
                 [make-snip (λ _ ld2)]))

(send fr show #t)
