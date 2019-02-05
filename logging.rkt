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

(define-syntax equ-pairs
  (syntax-rules ()
    [(_) '()]
    [(_ e more ...) (cons @~a{@'e = @e} (equ-pairs more ...))]))

; Log arbitrary expressions, showing both expression and its value.
(define-syntax-rule (log/e k exprs ...)
  (when (log-enabled? k)
    (let ([es (string-join (equ-pairs exprs ...) ", ")])
      (displayln @~a{@|k|: @es}))))

; Log arbitrary annotation.
(define-syntax-rule (log/a k args ...)
  (when (log-enabled? k)
    (let ([as (string-join (map ~a (list args ...)))])
      (displayln @~a{-- @as}))))
    
