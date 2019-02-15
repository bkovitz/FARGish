#lang typed/racket

(provide (all-defined-out))

(define-type (Hashof K V) (Immutable-HashTable K V))

(define-type Node (U Symbol Integer))  ; a node's id
(define-type Port-label (U Symbol Integer))
(define-type Port (List Node Port-label))
(define-type Edge/Set (Setof Port))  ; 2 ports
(define-type Edge/List (List Port Port))
(define-type Hop Edge/List)
(define-type Edge (U Edge/Set Edge/List))
(define-type Edge* (Setof Node))  ; 2 nodes
(define-type EdgeWeight (U Flonum Void))

; Name to show in visualization of a graph in place of node's id
(define-type DisplayName (U Symbol Integer String))

(define-type ATable (Hashof Node Flonum)) ; activations table

(: atable (->* () #:rest-star (Node Flonum) ATable))
(define atable hash)

(define integer? exact-integer?)

