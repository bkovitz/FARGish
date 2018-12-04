; search-item.rkt -- struct for an item to search for in the slipnet

#lang debug at-exp racket

(require "wheel.rkt"
         "observe.rkt")
(require racket/hash)
(require racket/flonum racket/unsafe/ops)
(require rackunit racket/pretty describe profile racket/enter racket/trace)

(provide (all-defined-out))

(struct search-item* (class number required?))
(struct target-class* (class))
(struct rejection-item* (node))
(struct inexact-number* (x exactness))

;; ======================================================================
;;
;; Constructors and simple predicates
;;

(define (search-item class n [required? #f])
  (search-item* class n required? #f))

(define rejection-item rejection-item*)

(define target-class target-class*)

(define (inexact-number n [exactness 0.1])
  (inexact-number* n exactness))

(define search-item? search-item*?)

(define rejection-item? rejection-item*?)

(define (required-item? s-item)
  (and (search-item*? s-item)
       (search-item*-required? s-item)))

(define inexact-number? inexact-number*?)

(define exact-number? number?)

;; ======================================================================
;;
;; Calculations
;;

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
(define (distance-metric n m)
  (cond
    [(void? n) 1.0]
    [(void? m) 1.0]
    [(exact-number? n)

