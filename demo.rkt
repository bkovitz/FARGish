#lang debug at-exp racket

(require rackunit data/collection racket/dict racket/generic racket/pretty
         racket/hash describe readline/readline
         "graph.rkt" "make-graph.rkt" "numbo0.rkt")


(let loop ()
  (newline)
  (define bricks (read (open-input-string
                         (format "(~a)" (readline "Bricks: ")))))
  (when (empty? bricks)
    (exit))
  (define target (read (open-input-string
                         (format "(~a)" (readline "Target: ")))))
  (when (not (= 1 (length target)))
    (exit))
  (run bricks (car target) big-slipnet)
  (loop))
