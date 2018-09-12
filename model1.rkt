#lang racket

(require "graph.rkt")

(define model (make-graph 'a 'b 'c '(tag next a b) '(tag next b c)))

