#lang racket/gui

(define WIDTH 500)
(define HEIGHT 500)

(define frame (new frame% [label "Snake"])) ; no size is specified

(define game-canvas
  (new canvas% [parent frame]
       [min-width WIDTH]
       [min-height HEIGHT]
       [stretchable-width #f] ; To make sure the canvas' sizes stay constant even if the frame is resized with the mouse
       [stretchable-height #f]
       [paint-callback
        (λ(cv dc)
          (send dc set-background "yellow")
          (send dc clear)
          )]))

; Before being shown, the sizes are automatically adjusted.
(send frame show #t)
