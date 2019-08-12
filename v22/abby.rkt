#lang racket

(define (square x) (* x x))

(define (ss x y z)
  (let ([top2 (take (sort (list x y z) >)
                    2)])
    (apply + (map square top2))))
