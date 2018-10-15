; farg-window.rkt -- Top-level window for FARG models

; HACK: Currently hard-coded for numbo0

#lang debug at-exp racket/gui

(require (prefix-in model: "numbo0.rkt") "xsusp3.rkt" "dots-canvas.rkt")
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

    (define slipnet-canvas (new dots-canvas% [parent this]))
    (define ws-canvas (new dots-canvas% [parent this] [color "Dark Orchid"]))))

(define (sanitized-activations ht)
  (for/hash ([(key activation) ht]
             #:when (> activation 0.1))
    (values key activation)))

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
