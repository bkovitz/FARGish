; fizzle.rkt -- Trappable run-time exceptions for FARGish

#lang typed/racket

(require racket/syntax racket/string syntax/parse syntax/parse/define
         (for-syntax racket/base racket/syntax syntax/stx syntax/parse))
(require/typed racket/base
               [srcloc->string (-> srcloc (U #f String))])
                 ; Why is this necessary?

(provide Fizzle?
         Fizzle-srcloc)

(begin-for-syntax
  (require racket/base racket/syntax racket/string syntax/parse
           syntax/parse/define)

  (define (srcloc-expr stx)
    #`(srcloc '#,(syntax-source stx)
              '#,(syntax-line stx)
              '#,(syntax-column stx)
              '#,(syntax-position stx)
              '#,(syntax-span stx)))
  
  (define (upcase-first stx)
    (let ([s (string->list (symbol->string (syntax-e stx)))])
      (list->string
        (cons (char-upcase (car s)) (cdr s)))))

  ;WHY is this necessary?
  (define (const x)
    (λ any x))

  (define-syntax-class fizzle-id
    #:description "The name of a fizzle macro must start with \"fizzle:\"."
    (pattern name:id
      #:when (string-prefix? (symbol->string (syntax-e #'name)) "fizzle:")
      #:attr struct-name (format-id #'name "~a" (upcase-first #'name)
                                    #:source #'name #:props #'name)
      #:attr struct-name? (format-id #'name "~a?" #'struct-name
                                     #:source #'name #:props #'name)
      #:attr make (format-id #'name "make-~a" #'struct-name
                             #:source #'name #:props #'name))))

(struct Fizzle exn:fail ([srcloc : srcloc])
        #:transparent
        #:property prop:exn:srclocs
        (λ (a-fizzle)
          (match a-fizzle
            [(struct Fizzle (msg marks a-srcloc))
             (list a-srcloc)])))

(define-syntax (define-fizzle stx)
  (syntax-parse stx
    [(_ name:fizzle-id (field:id ...) msg:expr)
     #`(begin
         (struct name.struct-name Fizzle ([field : Any] ...))
         (: name.make : Continuation-Mark-Set
                        srcloc
                        #,@(stx-map (const 'Any) #'(field ...))
                        -> name.struct-name)
         (define (name.make cms sl field ...)
           (name.struct-name (string-append
                               msg "\n" (or (srcloc->string sl) ""))
                             cms sl field ...))
         (define-syntax (name stx-)
           (syntax-parse stx-
             [(_ #,@#'(field ...))
              #`(raise (name.make (current-continuation-marks)
                                  #,(srcloc-expr stx-)
                                  field ...))]))
         (provide name name.struct-name?
                  ;field accessors
                  #,@(stx-map (λ (fld)
                                (string->symbol
                                  (format "~a-~a"
                                    (syntax->datum #'name.struct-name)
                                    (syntax->datum fld))))
                              #'(field ...))))]))

; To raise one of these exceptions, do, for example, (fizzle:nodeclass 'abc)

(define-fizzle fizzle:nodeclass (nodeclass)
  (format "unknown nodeclass: ~a" nodeclass))

(define-fizzle fizzle:tag-failed (tagspec nodes)
  (format "tagspec ~a does not apply to nodes ~a" tagspec nodes))

(define-fizzle fizzle:tagclass (tagclass)
  (format "unknown tagclass: ~a" tagclass))

(define-fizzle fizzle:not-a-tag (nodeclass)
  (format "Nodeclass ~a is not a tag and does not apply to other nodes."))

(define-fizzle fizzle:no-class (attrs)
  (format "No 'class provided in ~a" attrs))

(define-fizzle fizzle:class-not-symbol (attrs)
  (format "Value of 'class key is not a Symbol in ~a" attrs))

(define-fizzle fizzle:tag-arity (tagclass arity nodes)
  (format "Tagclass ~a has arity ~a but was applied to ~a."
          tagclass arity nodes))

;TODO UT
