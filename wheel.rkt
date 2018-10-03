; wheel.rkt -- Wheels to not reinvent

#lang debug at-exp racket

(provide atom? safe-car safe-cdr)

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (safe-car x)
  (match x
    [`(,a . ,d) a]
    [_ (void)]))

(define (safe-cdr x)
  (match x
    [`(,_ . ,d) d]
    [_ (void)]))

;;;based on version by soegaard, https://stackoverflow.com/a/45545280/1393162
;(define-match-expander dict
;  (λ (stx)
;     (syntax-case stx ()
;       [(_dict (
;; Doing this right looks very hard. The StackOverflow version doesn't call
;; dict-ref and consequently doesn't work for all values that support gen:dict.
;; It also doesn't match the whole dict analogously to hash-table.
