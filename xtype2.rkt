#lang typed/racket

(require "xtype.rkt")

(: atable (->* () #:rest-star (Node Flonum) ATable))
(define atable hash)

(define h (atable 'a 1.0))
