; logging.rkt -- Simple logger that lets you dynamically enable and disable
;                keys for what items to log

#lang at-exp racket

(provide (all-defined-out))

(define log-enabled (mutable-set))

(define (log-enable! . ks)
  (for ([k ks])
    (set-add! log-enabled k)))

(define (log-disable! . ks)
  (for ([k ks])
    (set-remove! log-enabled k)))

(define (log-enabled? k)
  (or (eq? 'any k)
      (set-member? log-enabled k)))

(define-syntax as-pairs
  (syntax-rules ()
    [(_) '()]
    [(_ e more ...) (cons (cons 'e e) (as-pairs more ...))]))

(define-syntax-rule (log/e k exprs ...)
  (when (log-enabled? k)
    (let ([es (string-join (map ~a (as-pairs exprs ...)))])
      (displayln @~a{@k @es}))))
