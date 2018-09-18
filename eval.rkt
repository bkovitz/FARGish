#lang racket

;; A test to see how to generate a matcher dynamically. This might be a way
;; to enable FARGish to convert tags defined in a spec into clauses in a
;; match statement.
(define code '(λ (x)
              (match x
                ['a 1]
                ['b 2]
                [_ (error "no")])))

(define-namespace-anchor a)
(define f (eval code (namespace-anchor->namespace a)))

(f 'a)
