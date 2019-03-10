#lang typed/racket

(require "typed-wheel.rkt")

(provide (all-defined-out))

(define-type Node (U Symbol Integer))  ; a node's id
(define-type Port-label (U Symbol Integer))
(define-type Port (List Node Port-label))
(define-type Edge/UPair (UnorderedPair Port))
(define-type Edge/List (List Port Port))
(define-type Hop Edge/List)
(define-type Edge (U Edge/UPair Edge/List))
(define-type Edge* (Setof Node))  ; 2 nodes
(define-type EdgeWeight Flonum)

; Name to show in visualization of a graph in place of node's id
(define-type DisplayName (U Symbol Integer String))

(define-type Attrs (Hashof Symbol Any)) ; attributes (of a Node, or anything)
(define-type ATable (Hashof Node Flonum)) ; activations table

(: atable (->* () #:rest-star (Node Flonum) ATable))
(define atable hash)

(define integer? exact-integer?)

(define-predicate edge/upair? Edge/UPair)

(define-predicate Node? Node)
