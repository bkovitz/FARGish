; Throwaway code: experimenting with racket/pict and racket/gui

#lang debug at-exp racket/gui

(require pict pict/color)

(define fr
  (new frame%
       [label "Example"]
       [width 300]
       [height 400]))

(define pict (inset (red (filled-rounded-rectangle 30 10)) 100))

(define (p! p)
  (set! pict p)
  (send fr refresh))
  
(define canvas
  (new canvas% [parent fr]
               [paint-callback
                 (λ (canvas dc)
                   (draw-pict pict dc 0 0))]))

(send fr show #t)
