#lang typed/racket

(provide Node Edge ATable)

(define-type Node (U Symbol Integer))
(define-type Edge (Setof Node))  ; 2 elements
(define-type ATable (Immutable-HashTable Node Flonum)) ; activations table

(struct s* ([x : Real] [y : Real]) #:prefab)
(: setw : Real (-> Real Real) -> s*)
(define (setw x f) (s* x (f x)))
(define s (setw 2.0 (λ (x) (* 3 x))))
