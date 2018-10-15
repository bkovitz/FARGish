; dot-canvas.rkt -- A canvas% of named dots
; 
; The diameter of each dot represents a number.

#lang debug at-exp racket/gui

(require racket/dict pict pict/color)
(require describe)

(provide (all-defined-out))

(struct cell (name pict) #:transparent)

(define dot-canvas%
  (class canvas%
    (init-field [virtual-width 1000]
                [max-diameter 40.0]
                [diameter-multiplier max-diameter]
                [color "Aquamarine"])
    (super-new)

    (inherit get-dc refresh)

    (define d-name->value '())
    (define pict (void))
    (define dirty? #t)

    (define/public (set-dots! dict)
      (set! d-name->value dict)
      (set! dirty? #t)
      (refresh))

    (define (make-cell pair)
      (match-define (cons -name value) pair)
      (define name (~a -name))
      (define t (inset (text name) 4 2))
      (define d (cc-superimpose
                  (ghost (disk max-diameter))
                  (disk (* diameter-multiplier value)
                      #:draw-border? #f
                      #:color color)))
      (define p (inset (vc-append t d) 10.0 10.0))
      (cell name p))

    (define (lay-out-rows picts)
      (for/fold ([p (blank)] [row (blank)] #:result (vl-append p row))
                ([item picts])
        (define item-width (pict-width item))
        (define row-width (pict-width row))
        (if (> (+ row-width item-width) virtual-width)
          (values (vl-append p row) item)
          (values p (hb-append row item)))))

    (define (dots-dict->pict d-name->value)
      (define cells (sort (for/list ([pair (dict->list d-name->value)])
                            (make-cell pair))
                          #:key cell-name
                          string<?))
      (lay-out-rows (map cell-pict cells)))

    (define/override (on-paint)
      (define dc (get-dc))
      (when dirty?
        (set! pict (dots-dict->pict d-name->value))
        (send this init-auto-scrollbars (exact-ceiling (pict-width pict))
                                        (exact-ceiling (pict-height pict))
                                        0.0 0.0)
        (set! dirty? #f))
      (draw-pict pict dc 0 0))))

(module* example racket/gui
  ;; To run in DrRacket:  (require (submod "dot-canvas.rkt" example)) (run)

  (require (submod ".."))
  (provide (all-defined-out))

  (define (run)
    (define frame (new frame% [label "dot-canvas example"]
                              [width 300]
                              [height 200]))
    (define canvas (new dot-canvas% [parent frame]
                                     [style '(hscroll vscroll)]))
    (send canvas set-dots! #hash((abc . 1.0) (archetype52 . 0.6)))
    (send frame show #t)))
