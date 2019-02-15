; typed-wheel.rkt -- Wheels to not reinvent, version for Typed Racket

#lang debug at-exp typed/racket

(require racket/syntax syntax/parse syntax/parse/define syntax/free-vars
         (for-syntax racket/syntax syntax/parse syntax/free-vars))

(provide (all-defined-out))

(define-syntax-rule (first-value expr)
  (call-with-values (λ () expr)
    (λ (result . ignored) result)))

; Is it possible to forward arguments like this in Typed Tracket?
;(: first-value/ (All (dom ...)
;                  (All (rng0 rng ...)
;                    (-> (->* (dom ...) (Values rng0 rng ...)) rng0))))
;(define (first-value/ f)
;  (λ args
;    (call-with-values (λ () (apply f args))
;      (λ ([result : rng0] . ignored) result))))
