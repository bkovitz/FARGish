; fizzle.rkt -- Trappable run-time exceptions for FARGish

#lang debug at-exp typed/racket

(require racket/syntax syntax/parse syntax/parse/define syntax/free-vars
         (for-syntax racket/syntax syntax/parse syntax/free-vars))
(require/typed racket/base
               [srcloc->string (-> srcloc (U #f String))])
                 ; Why is this necessary?

(provide fizzle:nodeclass
         Fizzle?)

(begin-for-syntax
  (require racket/syntax syntax/parse syntax/parse/define syntax/free-vars
           (for-syntax racket/base racket/syntax syntax/parse syntax/free-vars))
  (define (srcloc-expr stx)
    #`(srcloc '#,(syntax-source stx)
              '#,(syntax-line stx)
              '#,(syntax-column stx)
              '#,(syntax-position stx)
              '#,(syntax-span stx))))
;    (syntax-parse stx
;      [(_ e:expr)
;       (quasisyntax/loc #'e
;         (srcloc '#,(syntax-source #'e)
;                 '#,(syntax-line #'e)
;                 '#,(syntax-column #'e)
;                 '#,(syntax-position #'e)
;                 '#,(syntax-span #'e)))])))

(struct Fizzle exn:fail ([srcloc : srcloc])
        #:transparent
        #:property prop:exn:srclocs
        (λ (a-fizzle)
          (match a-fizzle
            [(struct Fizzle (msg marks a-srcloc))
             (list a-srcloc)])))

(struct Fizzle:nodeclass Fizzle ([nodeclass : Any]) #:transparent)

(struct Fizzle:tag-failed Fizzle ([tagclass : Any] [nodes : (Listof Any)])
                                 #:transparent)

(: make-Fizzle:nodeclass : Continuation-Mark-Set srcloc Any -> Fizzle:nodeclass)
(define (make-Fizzle:nodeclass cms sl nc)
  (Fizzle:nodeclass (format "unknown nodeclass: ~a\n~a" nc (srcloc->string sl))
                    cms
                    sl
                    nc))

(: make-Fizzle:tag-failed : Continuation-Mark-Set srcloc Any (Listof Any)
                            -> Fizzle:tag-failed)
(define (make-Fizzle:tag-failed cms sl tagspec nodes)
  (Fizzle:tag-failed (format "tagspec ~a does not apply to nodes ~a\n~a"
                             tagspec nodes (srcloc->string sl))
                     cms
                     sl
                     tagspec
                     nodes))

; Raises a Fizzle:nodeclass exception for nodeclass.
(define-syntax (fizzle:nodeclass stx)
  (syntax-parse stx
    [(_ nodeclass:expr)
     #`(raise (make-Fizzle:nodeclass (current-continuation-marks)
                                     #,(srcloc-expr stx)
                                     nodeclass))]))

; Raises a Fizzle:tag-failed exception: (fizzle:tag-failed tagclass nodes)
(define-syntax (fizzle:tag-failed stx)
  (syntax-parse stx
    [(_ tagspec:expr nodes:expr)
     #`(raise (make-Fizzle:tag-failed (current-continuation-marks)
                                      #,(srcloc-expr stx)
                                      tagspec
                                      nodes))]))

; TODO Redo everything so that all fizzles are defined by define-fizzle.
;(define-syntax (define-fizzle stx)
;  (syntax-parse stx
;    [(_ new-fizzle:id parent-fizzle:id (field:id ...) msg:expr)
;     #`(begin
;         (struct new-fizzle parent-fizzle ([field : Any] ...))
;         (define-syntax (new-fizzle sx)
;           (syntax-parse sx
;             [(_ field ...)
;(define-fizzle fizzle:nodeclass Fizzle (nodeclass)
;  (format "unknown nodeclass ~a" nodeclass))



;     #:with srcloc-expr (quasisyntax/loc #'nodeclass
;                          (srcloc '#,(syntax-source #'nodeclass)
;                                  '#,(syntax-line #'nodeclass)
;                                  '#,(syntax-column #'nodeclass)
;                                  '#,(syntax-position #'nodeclass)
;                                  '#,(syntax-span #'nodeclass)))
;     (quasisyntax/loc #'nodeclass
;       (let ([sl : srcloc #,(srcloc-expr #'nodeclass)]
;             [nc nodeclass])  ;nodeclass
;          (raise (Fizzle:nodeclass (format "unknown nodeclass: ~a\n~a"
;                                           nc
;                                           (srcloc->string sl))
;                                   (current-continuation-marks)
;                                   sl
;                                   nc))))]))

