; fargish.rkt  --  FARGish as a #lang? Implemented in Typed Racket?

#lang typed/racket

(require syntax/parse syntax/parse/define
         (for-syntax racket/syntax syntax/parse))
(require "types.rkt" "typed-wheel.rkt")

; Generate make-node in the main macro?

(struct Nodeclass ([name : Symbol]
                   ;[args : (Listof Symbol)]
                   [parents : (Listof Symbol)])
                  #:transparent)

(struct FARGishSpec ([class->parents : (Hashof Symbol (Setof Symbol))])
                    #:transparent)

;(define-syntax (farg-model-spec stx)
(define-syntax (define-spec stx)
  (define-syntax-class arg+type
    #:description "argument"
    #:datum-literals [:]
    (pattern [argname:id : type:expr]))

  (define-syntax-class name+args
    #:description "name maybe with arguments"
    #:attributes [name (arg 1) (arg.type 1)]
    (pattern name:id
             #:with (arg ...) #'()
             #:with (arg.type ...) #'())
    (pattern (name:id arg:arg+type ...)))

  (define-syntax-class nodeclass
    #:description "nodeclass"
    #:datum-literals [nodeclass]
    #:attributes [name (arg 1) (arg.type 1) (parent 1) value display-name]
    (pattern (nodeclass ~! decl:name+args body:nodeclass-body)
             #:with name #'decl.name
             #:with (arg ...) #'(decl.arg ...)
             #:with (arg.type ...) #'(decl.arg.type ...)
             #:with (parent ...) #'(body.parent ... ...)
             #:with value #'(~? body.value-expr #f)
             #:with display-name
                      (if (syntax-property #'body.display-name-expr 'missing?)
                        #''name
                        #'body.display-name-expr)
    ))

  (define-splicing-syntax-class nodeclass-body
    #:description "nodeclass body"
    #:datum-literals [is-a archetype value links-into applies-to]
    #:attributes [(parent 2) value-expr display-name-expr]
    (pattern (~seq
      (~alt (is-a ~! parent:expr ...+)
            (~optional (value ~! value-expr:expr)
                       #:too-many "'value' specified more than once"
                       #:defaults ([value
                                     (syntax-property #'(void) 'missing? #t)]))
            (~optional (display-name ~! display-name-expr:expr)
                       #:too-many "'display-name' specified more than once"
                       #:defaults ([display-name-expr
                                     (syntax-property #'(void) 'missing? #t)]))
            ) ...)
       #:do [(println #'(~? (parent ... ...) #f))]))

  (syntax-parse stx
    [(_ spec-name:id nc:nodeclass ...)
;     #:with (nc-struct ...)
;            #'((Nodeclass 'nc.name '(nc.arg ...) (list nc.parent ...)) ...)
;     #`(list nc-struct ...)
     ;#:with ht/class->is-a (format-id stx "ht/class->is-a" #:source stx)
     #`(begin
         (begin (: nc.name : nc.arg.type ... -> (Hashof Symbol Any))
                (define (nc.name nc.arg ...)
                  (let ([dispname : DisplayName nc.display-name])
                    (hash 'value nc.value
                          'display-name dispname
                          'class 'nc.name)))) ...
         (: spec-name : FARGishSpec)
         (define spec-name
           (FARGishSpec (hash (~@ 'nc.name (set 'nc.name 'nc.parent ...)) ...)))
         )]))

;(spec (nodeclass (blah a b c)))
;
;(spec (nodeclass blah (is-a 'x 'y) (is-a 'z)))
;
;(spec
;  (nodeclass blah
;    (value 22)))

(define-spec spec
  (nodeclass (number [n : Integer])
    (display-name n)
    (value n))
  (nodeclass plus))
(number 17)
;hey

