; Trying to figure out free-vars

#lang debug at-exp racket

(require syntax/parse syntax/parse/define syntax/free-vars
         (for-syntax syntax/parse syntax/free-vars))
(require racket/hash)
(require "wheel.rkt")
(require rackunit describe debug/repl)

(struct closure* (f free-vars body) #:transparent #:property prop:procedure 0)

(define (closure*-ref c name)
  (hash-ref (closure*-free-vars c) name (void)))

(define-syntax (λ stx)
  (syntax-parse stx
    [(_ args body0 body ...)
     (define expanded-lam (local-expand #'(lambda args body0 body ...)
                                         'expression
                                         '()))
     (syntax-parse expanded-lam
       #:literals (#%plain-lambda)
       [(~and whole (#%plain-lambda (arg ...) body))
        (with-syntax ([(fv ...) (free-vars #'whole)])
          #`(closure* #,expanded-lam
                      (hash (~@ 'fv fv) ...)
                      '#,stx))])]))

(define (f x) (λ (a) (list a x)))
(define g (f 'xxx))
(closure*-body g)
(closure*-free-vars g)
(closure*-ref g 'x)
