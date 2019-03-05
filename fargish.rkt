; fargish.rkt  --  FARGish as a #lang? Implemented in Typed Racket?

#lang typed/racket

(require syntax/parse syntax/parse/define
         (for-syntax syntax/parse))
(require "wheel.rkt")

; Generate make-node in the main macro?

(struct Nodeclass ([name : Symbol]
                   [args : (Listof Symbol)]
                   [parents : (Listof Symbol)])
                  #:transparent)

;(define-syntax (farg-model-spec stx)
(define-syntax (spec stx)
  (define-syntax-class name+args
    #:description "name or name with arguments"
    #:attributes [name (arg 1)]
    (pattern name:id
             #:with (arg ...) #'())
    (pattern (name:id arg:id ...)))

  (define-syntax-class nodeclass
    #:description "nodeclass"
    #:datum-literals [nodeclass]
    #:attributes [name (arg 1) (parent 1) value]
    (pattern (nodeclass ~! decl:name+args body:nodeclass-body)
             #:with name #'decl.name
             #:with (arg ...) #'(decl.arg ...)
             #:with (parent ...) #'(body.parent ... ...)
             #:with value #'(~? body.value-expr #f)
    ))

  (define-splicing-syntax-class nodeclass-body
    #:description "nodeclass body"
    #:datum-literals [is-a archetype value links-into applies-to]
    #:attributes [(parent 2) value-expr]
    (pattern (~seq
      (~alt (is-a ~! parent:expr ...+)
            (~optional (value ~! value-expr:expr)
                       #:too-many "'value' specified more than once")
            ) ...)
       #:do [(println #'(~? value-expr #f))]))

  (syntax-parse stx
    [(_ nc:nodeclass ...)
     #:with (nc-struct ...)
            #'((Nodeclass 'nc.name '(nc.arg ...) (list nc.parent ...)) ...)
     #`(list nc-struct ...)]))

(spec (nodeclass (blah a b c)))

(spec (nodeclass blah (is-a 'x 'y) (is-a 'z)))

(spec
  (nodeclass blah
    (value 22)))
