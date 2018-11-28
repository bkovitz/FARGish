; observe.rkt -- Record and retrieve numerical observations

#lang debug at-exp racket

(require "wheel.rkt")
(require data/gvector racket/hash syntax/parse/define)
(require plot pict pict/convert)
(require rackunit racket/pretty describe profile)

;(provide os os-n os-prefix observing observe! see)

(define os-prefix (make-parameter '())) ; list of (key value)
  ;TODO should guard that parameter is a list of key-value pairs?

(define os (make-gvector)) ; "observations"
(define os-n (void)) (set! os-n 0)

(define-simple-macro (observing keyspec body:expr ...+)
  (parameterize ([os-prefix (safe-append (os-prefix) keyspec)])
    body ...))

#;(define (observe! . args)
  (when (null? args)
    (raise-arguments-error 'observe!
      "need at least one argument"))
  (let*-values ([(keys x) (split-at-right args 1)]
                [(keys) (if (null? keys)
                          (let ([n os-n])
                            (set! os-n (add1 n))
                            n)
                          keys)]
                [(x) (car x)])
    (hash-set! os (safe-append (os-prefix) keys) x)
    x))

(define (observe! k v)
  (gvector-add! os (append-item (os-prefix) (list k v))))

; NEXT (see . keys)

