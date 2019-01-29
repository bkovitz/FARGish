; Trying to figure out free-vars

#lang debug at-exp racket

(require syntax/parse syntax/parse/define syntax/free-vars
         (for-syntax syntax/parse syntax/free-vars))
(require racket/hash)
(require "wheel.rkt")
(require rackunit)

(struct c* (f free-vars) #:prefab)

(define-syntax (cλ stx)
  (syntax-parse stx
    [(_ args body0 body ...)
     (define expanded-lam (local-expand #'(lambda args body0 body ...)
                                         'expression
                                         '()))
     (displayln expanded-lam)
     (syntax-parse expanded-lam
       #:literals (#%plain-lambda)
       [(#%plain-lambda (arg ...) body)
        (displayln (free-vars #'expanded-lam))
        expanded-lam])]))

(lambda (x) (cλ (a) (list a x)))
; ()   Why not (x)?
