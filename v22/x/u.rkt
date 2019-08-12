; Throwaway experimentation to understand Units in Racket

#lang debug at-exp racket

(require rackunit data/collection racket/dict racket/generic racket/pretty
         racket/hash describe
         "graph.rkt" "make-graph.rkt")

(define-signature sig^
  (func))

(define-unit u1@
  (import)
  (export sig^)

  (define (func . args)
    (list args 1)))

(define-unit u2@
  (import)
  (export sig^)

  (define (func . args)
    (list args 2)))

(let ()
  (define-values/invoke-unit/infer u1@)
  (func 'a))

(let ()
  (define-values/invoke-unit/infer u2@)
  (func 'a))
