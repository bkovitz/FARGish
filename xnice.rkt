; xnice.rkt -- Experimenting with making nice syntax for specifying a FARG model

#lang debug at-exp racket

(define empty-set (set))
(define empty-hash (hash))

(struct is-a* (ancestors) #:prefab)
; ancestors: (Setof Symbol)

(define (is-a . args)
  (is-a* (list->set args)))

(struct by-ports* (from-port to-port) #:prefab)

(define by-ports by-ports*)

(struct links-into* (ctx-class by-portss) #:prefab)
;ctx-class : Symbol
;by-portss : (Listof by-ports)

(define (links-into ctx-class . by-portss)
  (links-into* ctx-class by-portss))

(struct nodeclass* (name ancestors links-intos) #:prefab)

(define (make-nodeclass name . elems)
  (for/fold ([ancestors empty-set]
             [links-intos empty-hash]
             #:result (nodeclass* name ancestors links-intos))
            ([elem elems])
    (match elem
      [(is-a* ancestors-)
       (values (set-union ancestors ancestors-) links-intos)]
      [(links-into* ctx by-portss)
       (values ancestors (hash-set links-intos ctx by-portss))])))

(define-syntax-rule (nodeclass name elems ...)
  (make-nodeclass (quote name) elems ...))

(define (tag-applies-to? (need source) . nodes)
  (let ([pred (λ (node)
                (no-neighbor-at-port?/g 'source node))])
    (apply pred nodes)))

(struct applies-to* (taggees conditions))

(define-syntax applies-to
  (syntax-rules (condition)
    [(applies-to ([taggee taggee-info ...] ...)
       (condition condition-expr) ...)
     (applies-to* (list (make-taggee 'taggee taggee-info ...) ...)
                  (list (λ (taggee ...)
                          condition-expr) ...))]))

