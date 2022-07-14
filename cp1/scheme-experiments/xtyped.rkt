; xtyped.rkt -- Just experimenting with Typed Racket

#lang typed/racket

(struct pt ([x : Real] [y : Real]))

(: distance (-> pt pt Real))
(define (distance p1 p2)
  (sqrt (+ (sqr (- (pt-x p2) (pt-x p1)))
           (sqr (- (pt-y p2) (pt-y p1))))))

(define-type Tree (U leaf node))
(struct leaf ([val : Number]))
(struct node ([left : Tree] [right : Tree]))

(define (add x y)
  (+ x y))

(define-type Painter (List Any Any Any))

(: run-painter (-> Painter Void))
(define (run-painter painter)
  (match painter
    [(list source target func)
     (display target)]))
