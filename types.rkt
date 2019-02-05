#lang typed/racket

(provide (all-defined-out))

(define-type (Hashof K V) (Immutable-HashTable K V))

(define-type Node (U Symbol Integer))
(define-type Port-label (U Symbol Integer))
(define-type Port (Listof Node Port-label))
(define-type Edge/Set (Setof Port))  ; 2 ports
(define-type Edge/List (Listof Port Port))
(define-type Edge (U Edge/Set Edge/List))
(define-type Edge* (Setof Node))  ; 2 nodes

(define-type ATable (Hashof Node Flonum)) ; activations table

(: atable (->* () #:rest-star (Node Flonum) ATable))
(define atable hash)
