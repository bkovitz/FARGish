#lang typed/racket

(struct s* ([x : Real] [y : Real]) #:prefab)
(: setw : Real (-> Real Real) -> s*)
(define (setw x f) (s* x (f x)))
(define s (setw 2.0 (λ (x) (* 3 x))))