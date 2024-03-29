; sa-display.rkt -- A canvas that displays spreading activation in progress

;NEXT In the canvas, store a box containing the FARG model, or something to
;call to generate the next timestep of activations.

#lang debug at-exp racket/gui

(require pict pict/color)
(require describe)

(provide (all-defined-out))

(define WIDTH 1000)

(struct cell (nodeid name text disk pict) #:transparent)
(struct layout (cells pict) #:transparent)

(define (make-cell pair)
  (match-define (cons nodeid activation) pair)
  (define name (archetype-symbol->s nodeid))
  (define t (inset (text name) 0.0 12.0))
  (define diameter (* 10.0 (sqrt activation)))
  (define d (disk diameter #:draw-border? #f #:color "Aquamarine"))
  (define p (inset
              (vc-append t d)
              10.0 10.0))
  (cell nodeid name t d p))

(define (lay-out-rows items [max-width WIDTH])
  (for/fold ([pict (blank)] [row (blank)] #:result (vl-append pict row))
            ([item items])
    (define item-width (pict-width item))
    (define row-width (pict-width row))
    (if (> (+ row-width item-width) max-width)
      (values (vl-append pict row) item)
      (values pict (hb-append row item)))))

(define (strip-prefix s prefix)
  (if (string-prefix? s prefix)
    (substring s (string-length prefix))
    #f))

(define (archetype-symbol->s sym)
  (define s (~a sym))
  (or (strip-prefix s "archetype-")
      (strip-prefix s "archetype")
      s))

(define (activations->pict activations)
  (define cells (sort (for/list ([pair (hash->list activations)])
                        (make-cell pair))
                      #:key cell-name
                      string<?))
  (define ly (layout cells (lay-out-rows (map cell-pict cells))))
  (layout-pict ly)) ;TODO draw lines

(define sa-canvas%
  (class canvas%
    (inherit get-dc refresh)

    (init-field controller)

    (define activations #hash())
    (define pict (void))
    (define dirty? #t)

    (init-field [min-width 400]
                [min-height 300])
    (super-new [min-width min-width]
               [min-height min-height])

    (send this init-auto-scrollbars 1000 2000 0.0 0.0)

    (send controller set-view this)

    (define/public (set-activations! ht)
      (set! activations #R ht)
      (set! dirty? #t)
      (refresh))

    (define/override (on-char ke)
      (case (send ke get-key-code)
        [(#\space) (send controller make-new-activations)]))

    (define/override (on-paint)
      (define dc (get-dc))
      (when dirty?
        (set! pict (activations->pict activations))
        (set! dirty? #f))
      (draw-pict pict dc 0 0))))

(define (filter-activations activations)
  (for/hash ([(key a) activations]
             #:when (> a 0.1))
    (values key a)))

(module* example racket/gui
  ;; To run in DrRacket:  (require (submod "sa-display.rkt" example))

  (require (submod ".."))
  (require "numbo0.rkt"  "xsusp2.rkt")

  (define controller%
    (class object%
      (super-new)

      (init-field [view (void)])

      (define get-next-activations (suspended (run '(2 2) 4 big-slipnet)))

      (define/public (set-view new-view)
        (set! view new-view))

      (define/public (make-new-activations)
        (send canvas set-activations!
              (filter-activations (get-next-activations))))))

  (define is '#hash(
    (archetype-fills-port-15-source . 1.0)
    (archetype-fills-port-4-result . 1.0)
    (archetype-fills-port-5-result . 1.0)
    (archetype-fills-port-6-result . 1.0)
    (archetype-fills-port-greater-result-4 . 1.0)
    (archetype-fills-port-greater-result-5 . 1.0)
    (archetype-fills-port-greater-result-6 . 1.0)
    (archetype15 . 1.0)
    (archetype4 . 1.0)
    (archetype5 . 1.0)
    (archetype6 . 1.0)))

  (define frame (new frame% [label "Spreading activation"]))
  (define canvas (new sa-canvas%
                      [parent frame]
                      [controller (new controller%)]
                      [style '(hscroll vscroll)]))

  ;(send canvas set-activations! is)
  (send frame show #t)
)
