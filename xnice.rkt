; xnice.rkt -- Experimenting with making nice syntax for specifying a FARG model

#lang debug at-exp racket

(define empty-set (set))
(define empty-hash (hash))

(struct is-a (ancestors) #:prefab #:constructor-name make-is-a)
; ancestors: (Setof Symbol)

(define (is-a . args)
  (make-is-a (list->set args)))

(struct by-ports (from-port to-port) #:prefab)

(struct links-into (ctx-class by-portss) #:prefab
                   #:constructor-name make-links-into)
;ctx-class : Symbol
;by-portss : (Listof by-ports)

(define (links-into ctx-class . by-portss)
  (make-links-into ctx-class by-portss))

(struct nodeclass (name ancestors links-intos) #:prefab
                  #:constructor-name make-nodeclass)

(define (mk-nodeclass name . args)
  (for/fold ([ancestors empty-set]
             [links-intos empty-hash]
             #:result (make-nodeclass name ancestors links-intos))
            ([arg args])
    (match arg
      [(is-a ancestors-)
       (values (set-union ancestors ancestors-) links-intos)]
      [(links-into ctx by-portss)
       (values ancestors (hash-set links-intos ctx))])))
