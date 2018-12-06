#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define)

(define-ffi-definer define-a (ffi-lib "a.o"))
(define-a func (_fun _int -> _int))

(func 3)


