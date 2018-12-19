; search-item.rkt -- struct for an item to search for in the slipnet

#lang debug at-exp racket

(require "wheel.rkt"
         "observe.rkt")
(require racket/hash)
(require racket/flonum racket/unsafe/ops)
(require rackunit racket/pretty describe profile racket/enter racket/trace)

(provide (all-defined-out))

(struct search-item* (class number required?) #:prefab)
(struct target-class* (class) #:prefab)
(struct rejection-item* (node) #:prefab)
(struct inexact-number* (x exactness) #:prefab)

;; ======================================================================
;;
;; Constructors and simple predicates
;;

(define (search-item class [n (void)] [required? #f])
  (search-item* class n required? #f))

(define rejection-item rejection-item*)

(define target-class target-class*)

(define (inexact-number n [exactness 0.1])
  (inexact-number* n exactness))

(define search-item? search-item*?)
(define rejection-item? rejection-item*?)
(define target-class? target-class*?)

(define (required-item? s-item)
  (and (search-item*? s-item)
       (search-item*-required? s-item)))

(define inexact-number? inexact-number*?)

(define exact-number? number?)

;; ======================================================================
;;
;; Calculations
;;

(define (search-item->archetype-name search-item)
  (define v (search-item->v))
  (cond
    [(void? v) (void)]
    [else (f:archetype-name v)]))

(define (search-item->v s-item)
  (define n (->exact-number s-item))
  (cond
    [(not (search-item? s-item))
     (void)]
    [(void? n)
     (search-item*-class s-item)]
    [else
     (define class (search-item*-class s-item))
     `(,class ,n)]))

(define (->exact-number x)
  (cond
    [(number? x) x]
    [(inexact-number? x) (inexact-number*-n x)]
    [(search-item? x) (->exact-number (search-item*-n x))]
    [(rejection-item? x) (void)]
    [(target-class? x) (void)]))

(define (acceptability g s-item node)
  (cond
     [(rejection-item? s-item)
      (cond
        [(equal? node (rejection-item*-node s-item)) 0.0]
        [else 1.0])]
     [(required-item? s-item)
      (number-match g s-item node)]
     [(pair? s-item)
      (for/product ([item s-item])
        (acceptability g item node))]))

; Returns number in range 0.0..1.0. 1.0 means exact match; 0.0 means total
; non-match.
(define (distance/unit-interval n m)
  (cond
    [(void? n) 1.0]
    [(void? m) 1.0]
    [(and (exact-number? n) (exact-number? m))
     (if (= n m) 1.0 0.0)]
    [else
     (define absolute-difference (abs (- (->number n) (->number m))))
     (define greatest-exactness (max (exactness n) (exactness m)))
     (cond
       [(< greatest-exactness 0.01) 1.0]
       [else (exp (- (* greatest-exactness absolute-difference)))])]))

