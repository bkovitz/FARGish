; dots-canvas.rkt -- A canvas% of named dots
; 
; The diameter of each dot represents a number.

#lang debug at-exp racket/gui

(require racket/dict pict pict/color)
(require describe)

(provide (all-defined-out))

(struct cell (name pict) #:transparent)

(define dots-canvas%
  (class canvas%
    (init-field [virtual-width 1000]
                [max-diameter 20.0]
                [diameter-multiplier max-diameter]
                [color "Aquamarine"]
                [style '()])
    (super-new [style (list* 'border 'hscroll 'vscroll style)])

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
      (define t (inset (text name) 4 3))
      (define d (cc-superimpose
                  (ghost (disk max-diameter))
                  (disk (* diameter-multiplier value)
                      #:draw-border? #f
                      #:color color)))
      (define p (inset (vc-append t d) 2 6))
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
        (set! pict (dots-dict->pict d-name->value)))
      (draw-pict pict dc 0 0)
      (when dirty?
        (define width (exact-ceiling (pict-width pict)))
        (define height (exact-ceiling (pict-height pict)))
        (when (and (not (zero? width)) (not (zero? height)))
          (send this init-auto-scrollbars width height 0.0 0.0))
        (set! dirty? #f)))))

(module* example racket/gui
  ;; To run in DrRacket:  (require (submod "dots-canvas.rkt" example)) (run)

  (require (submod ".."))
  (provide (all-defined-out))

  (define (run)
    (define frame (new frame% [label "dots-canvas example"]
                              [width 300]
                              [height 200]))
    (define canvas (new dots-canvas% [parent frame]
                                     [style '(hscroll vscroll)]
                                     [virtual-width 300]))
    (send canvas set-dots!
          #hash((abc . 1.0) (archetype52 . 0.6) (zipdot . 0.1)
                (x . 0.15) (bcdef-dot . 0.2) (this-dot . 0.3)))
    (send frame show #t)))
