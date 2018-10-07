; Throwaway code: experimenting with racket/gui

#lang debug at-exp racket/gui

(define mw (new frame% [label "vishop"]))
 
(define our-canvas%
  (class canvas%
    (define/override (on-char ke)
      (eprintf "got ~v\n" (send ke get-key-code)))
    (super-new)))
 
(define buffer (new our-canvas% [parent mw]))
(send buffer focus)
 
(send mw create-status-line)
(send mw show #t)
