; xsyntax.rkt -- Throwaway code to experiment with syntax-parse

#lang debug at-exp racket

(require (for-syntax racket/syntax syntax/parse racket)
         (for-template racket)
         racket/syntax)
(require rackunit debug/repl describe)

(define-syntax (spec stx)
  (define-syntax-class nodeclass-elem
    #:description "nodeclass element"
    #:attributes ((parent 1))
    (pattern ((~literal is-a) parent:expr ...+))
    (pattern ((~literal value) value:expr))
  )

  (define-syntax-class nodeclass
    (pattern (nodeclass (name:id arg:id ...) elem:nodeclass-elem ...)
             #:with (parent ...) #'(elem.parent ... ...)
             ))

  (syntax-parse stx
    [(_ nc:nodeclass ...)
     #`(list '(nc.name nc.parent ...) ...)]))
