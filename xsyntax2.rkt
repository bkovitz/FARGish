#lang debug at-exp racket

;(define-syntax nodeclass
;  (nodeclass (args ...) elems ...)
;
;  (λ (args ...) elems ...)

(require syntax/parse syntax/parse/define
         (for-syntax syntax/parse))

(define-syntax (nodeclass stx)
  (define-splicing-syntax-class nodeclass-elems
    #:datum-literals [is-a archetype]
    #:attributes [(parent-expr 2) (archetype-expr 2)]
    (pattern (~seq (~alt (is-a ~! parent-expr:expr ...+)
                         (archetype ~! archetype-expr:expr ...+)) ...)))

  (syntax-parse stx
    [(nodeclass (name:id arg:id ...) elems:nodeclass-elems)
     #'(hash 'args '(arg ...)
             'is-a (list elems.parent-expr ... ...)
             'archetype (list elems.archetype-expr ... ...))]))

(nodeclass (number n)
  (is-a 'parent)
  (archetype 'a))

