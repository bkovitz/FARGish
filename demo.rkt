#lang debug at-exp racket

(require rackunit data/collection racket/dict racket/generic racket/pretty
         racket/hash describe readline/readline
         "graph.rkt" "make-graph.rkt" "numbo0.rkt")


(for ([_ (in-naturals 1)])
  (define bricks (read (open-input-string
                         (format "(~a)" (readline "Bricks: ")))))
  #:break (empty? bricks)
  (define target (read (open-input-string
                         (format "(~a)" (readline "Target: ")))))
  #:break (empty? target)
  (run bricks (car target) big-slipnet))