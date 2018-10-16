; farg-window.rkt -- Top-level window for FARG models

; HACK: Currently hard-coded for numbo0

#lang debug at-exp racket/gui

(require (prefix-in model: "numbo0.rkt") "xsusp3.rkt" "dots-canvas.rkt")
(require framework)
(require describe)
(provide (all-defined-out))

(define farg-window%
  (class frame%
    (init-field controller
                [label "FARG model"]
                [slipnet-activations '()]
                [width 600]
                [height 400])
    (super-new [label label] [width width] [height height])

    (send controller set-view! this)

    (define/override (on-subwindow-char receiver ke)
      (if (eq? this receiver)
        (begin
          (case (send ke get-key-code)
            [(#\space) (send controller continue)])
          #t)
        #f))

    (define/public (set-slipnet-activations! as)
      (send slipnet-canvas set-dots! (sanitized-activations as)))

    (define/public (set-saliences! ss)
      (send ws-canvas set-dots! (sanitized-activations ss)))

    (define vp (new panel:vertical-dragable% [parent this]))
    (define slipnet-canvas (new dots-canvas% [parent vp]))
    (define ws-canvas (new dots-canvas% [parent vp] [color "Dark Orchid"]))))

(define (sanitized-activations ht)
  (for/hash ([(key activation) ht]
             #:when (> activation 0.1))
    (values key activation)))

(define g (void)) ; current graph, i.e. current FARG model

(define controller%
  (class object%
    (init-field model-runner
                [view (void)])
    (super-new)

    (define/public (set-view! -view)
      (set! view -view))

    (define/public (continue)
      (match (model-runner)
        [`(slipnet-activations ,ht)
          (when view
            (send view set-slipnet-activations! ht))]
        [`(g ,new-g) ; new graph
          (set! g new-g)
          (continue)]
        [`(done ,result)
          (send view create-status-line)
          (send view set-status-text (~a result))]
        [`(numbo-ws ,ht)
          (when view
            (send view set-saliences! ht))]
        [got
          (displayln
            @~a{Unknown suspension result from model-runner: @got})]))))

(define controller (void))
(define frame (void))

(define (run . args)
  (set! controller (new controller%
                       [model-runner (suspended (apply model:run args))]))
  (set! frame (new farg-window% [controller controller]))
  (send frame show #t))

;; To run in DrRacket:
;;   (run '(4 5 6) 15 model:big-slipnet)
