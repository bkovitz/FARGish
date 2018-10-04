; Throwaway code: experimenting with racket/gui

#lang debug at-exp racket/gui

(define application-frame
  (new frame%
    [label "Example"]
    [width 400]
    [height 300]))

(define frame
  (new frame%
       [label "Example"]
       [width 300]
       [height 300]))

(define dark-orchid (send the-color-database find-color "Dark Orchid"))

(define num-invocations 0)

(define to-show
  `((set-scale 3 3)
    (set-text-foreground "blue")
    (draw-text "Some text" 0 0)))

(define (args! . args)
  (set! to-show args)
  (send frame refresh))

(define (+arg! . args)
  (set! to-show (append to-show args))
  (send frame refresh))

(define bs!  ; "backspace"
  (case-lambda
    [(n)
     (set! to-show (drop-right to-show 1))
     (send frame refresh)]
    [()
     (bs! 1)]))

(define canvas
  (new canvas% [parent frame]
               [paint-callback
                 (λ (canvas dc)
                   (for ([args to-show])
                     (apply dynamic-send dc args)))

                 #;(λ (canvas dc)
                   (set! num-invocations (add1 num-invocations))
                   (send dc set-scale 3 3)
                   (send dc set-text-foreground "blue")
                   (send dc draw-text @~a{Invocation @num-invocations} 0 0))]))
(send canvas set-canvas-background dark-orchid)
(send frame show #t)
