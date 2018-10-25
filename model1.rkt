; model1.rkt -- Generic run-time code for FARG model
;
; Goes with numbo1.rkt. Probably will obsolete soon after numbo1.rkt is done.

#lang debug at-exp racket

(require errortrace)
(require "wheel.rkt" "xsusp3.rkt" "fargish.rkt" (prefix-in g: "graph.rkt")
         (prefix-in g: "make-graph.rkt")
         (only-in "graph.rkt" pr-graph pr-group pr-node))
(require racket/set racket/hash)
(require rackunit racket/pretty describe)

(provide (all-defined-out))

;; Global constants

;TODO Global constants should be stored as variables in the graph.
(define max-timesteps 20)
(define slipnet-spreading-rate 0.01)
(define slipnet-decay 0.9)
(define slipnet-timesteps 3)
(define support-decay-rate 0.5)
