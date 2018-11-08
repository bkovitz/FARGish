#lang debug at-exp racket

;(define-syntax nodeclass
;  (nodeclass (args ...) elems ...)
;
;  (λ (args ...) elems ...)

(require syntax/parse syntax/parse/define
         (for-syntax syntax/parse))
(require "wheel.rkt")

(struct nodeclass* (name class-attrs ancestors) #:prefab)

(struct by-ports* (from-port-label to-port-label) #:prefab)

(struct links-into* (ctx-class by-portss) #:prefab)
;ctx-class : Symbol
;by-portss : (Listof by-ports)

(define-syntax (nodeclass stx)
  (define-syntax-class name-and-args
    #:description "nodeclass name or name with arguments"
    #:attributes [(name 0) (arg 1)]
    (pattern name:id #:with (arg ...) #'())
    (pattern (name:id arg:id ...)))

  (define-splicing-syntax-class li-body
    #:description "body of links-into"
    #:attributes [(ctx 0) (by-ports 1)]
    (pattern (~seq ctx:expr by-ports:expr ...)))

  (define-splicing-syntax-class taggee-body
    #:description "taggee"
    #:datum-literals [of-class]
    #:attributes [(name 0) (of-class-expr 2) (by-ports 1)]
    (pattern (~seq name:id
                   (~alt (of-class ~! of-class-expr:expr ...)
                         by-ports:expr) ...)))

;  (define-splicing-syntax-class applies-to-body
;    #:description "body of applies-to"
;    #:attributes [(taggee 1) (condition 1)]
;    (pattern (~seq ([taggee:taggee-body] ...)
;               (condition condition-expr:expr ...+))))

  (define-splicing-syntax-class nodeclass-elems
    #:description "nodeclass elements"
    #:datum-literals [is-a archetype name value links-into]
;    #:attributes [(parent-expr 2) (archetype-expr 2) (name-expr 1)
;                  (value-expr 1) (li-expr 1)]
    #:auto-nested-attributes
    (pattern (~seq (~alt (is-a ~! parent-expr:expr ...+)
                         (archetype ~! archetype-expr:expr ...+)
                         (name ~! name-expr:expr)
                         (value ~! value-expr:expr)
                         (links-into ~! li-expr:li-body)
                         ) ...)))

  (syntax-parse stx
    ;[(nodeclass (name:id arg:id ...) elems:nodeclass-elems)
    [(nodeclass nm:name-and-args elems:nodeclass-elems)
     #`(hash 'name 'nm.name
             'args '(nm.arg ...)
             'parents (list elems.parent-expr ... ...)
             'display-name (list (λ (nm.arg ...) elems.name-expr) ...)
             'archetypes (list (λ (nm.arg ...) elems.archetype-expr ...) ...)
             'value (list (λ (nm.arg ...) elems.value-expr) ...)
             'links-into
               (list (λ (nm.arg ...)
                       (links-into* elems.li-expr.ctx
                                    (list elems.li-expr.by-ports ...)) ...))
             )]))

(nodeclass (number n)
  (is-a 'parent)
  (name n)
  (links-into 'ctx 'by-ports)
  (archetype 'a))

;(define (make-nodeclass class-attrs)
;  (let ([class-attrs (
;  (define class-attrs
;    (hash 'name (hash-ref ht 'name)
;          'args (hash-ref ht 'args)
;          'parents (hash-ref ht 'is-a)
;          'archetypes (hash-ref

