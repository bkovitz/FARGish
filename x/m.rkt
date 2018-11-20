#lang racket

(provide (all-from-out 'inner))

(module inner racket
  (provide abc)
  (define abc 123))

(require 'inner)

(module main racket
  (require (submod ".." inner))
  (print abc))
