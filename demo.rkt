#lang debug at-exp racket

(require rackunit data/collection racket/dict racket/generic racket/pretty
         racket/hash describe readline/readline
         "graph.rkt" "make-graph.rkt" "numbo0.rkt")


(define bricks (read (open-input-string (format "(~a)" (readline "Bricks: ")))))
(define target (read (open-input-string (readline "Target: "))))
(define g (run bricks target slipnet))
