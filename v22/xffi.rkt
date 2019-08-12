#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/vector)

(define ARRAY (_array _double 10))

(define-ffi-definer define-a (ffi-lib "a.so"))
;(define-a func (_fun _int -> _int))
;(define-a update (_fun (_ptr io ARRAY) -> _void))
(define-a update (_fun _pointer -> _void))
; I also tried _ptr modes i and o

;(func 3)

;(define a (ptr-ref (malloc ARRAY 'atomic) ARRAY))
;(array-set! a 0 3.0)
;(update a)
;(array-ref a 0)

; No effect: the result is 3.0, not 6.0

(define v64 (make-f64vector 10))
(f64vector-set! v64 0 3.0)
(update (f64vector->cpointer v64))
(f64vector-ref v64 0)
