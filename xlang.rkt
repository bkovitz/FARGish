#lang racket/base

;(provide (all-from-out racket/base))

;(provide (except-out (all-from-out racket/base) #%app #%module-begin))

; All the core forms of Racket
(provide ;#%app
         begin
         begin0
         begin-for-syntax
         case-lambda
         #%datum
         #%declare
         define
         define-values
         define-syntaxes
         #%expression
         if
         let-values
         letrec-values
         ;#%module-begin
         module
         module*
         #%plain-app
         #%plain-lambda
         #%plain-module-begin
         #%provide
         quote
         quote-syntax
         #%require
         set!
         #%top
         #%top-interaction
         with-continuation-mark
         #%variable-reference
         )

(provide (rename-out [displayln d])
         h
         )

(provide (rename-out [xlang-app #%app]
                     [xlang-module-begin #%module-begin]
                     ))

(define-syntax-rule (xlang-app f arg ...)
  (begin
    (displayln "APP")
    (#%app f arg ...)))

(define-syntax-rule (xlang-module-begin arg ...)
  (#%module-begin (displayln "MB") arg ...))

;(define h hash)

(define-syntax-rule (h name args ...)
  (define name (hash args ...)))
