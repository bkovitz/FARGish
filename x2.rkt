#lang racket

(define (any-double? l)
  (for/or ([i (in-list l)])
    (for/or ([i2 (in-list l)])
      (= i2 (* 2 i)))))

(define l1 (for/list ([i (in-range 5000)])
             (+ (* 2 i) 1)))
(define l2 (for/list ([i (in-range 5000)])
             (- (* 2 i) 1)))

(time (or (any-double? l1) (any-double? l2)))

(time (let ([f (future (thunk (any-double? l1)))]
        [g (future (thunk (any-double? l2)))])
    (or (touch f) (touch g))))

(time (let ([f (future (lambda () (any-double? l2)))])
  (or (any-double? l1)
      (touch f))))