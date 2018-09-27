#lang debug at-exp racket

(require rackunit data/collection racket/dict racket/generic racket/pretty
         racket/hash describe readline/readline
         "graph.rkt" "make-graph.rkt" "numbo0.rkt")


(define bricks (read (open-input-string (format "(~a)" (readline "Bricks: ")))))
(define target (read (open-input-string (readline "Target: "))))
(define g (let*-values ([(g) (make-graph)]
                        [(g) (make-numbo-ws g '(4 5 6) 15)]
                        [(g _) (copy-graph-into-graph g slipnet)])
            g))
(define h (run g))

