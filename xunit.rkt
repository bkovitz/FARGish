; Throwaway code: figuring out units in Racket

; This demonstrates overriding some but not all of the exports from a unit.

#lang debug at-exp racket

(define-signature s1^
  (a b))

(define-unit u1@
  (import)
  (export s1^)

  (define a 'aa)
  (define b 'bb))

(define-unit u2@
  (import (prefix 1: s1^))
  (export s1^)
  (define a 1:a)
  (define b (cons 'b 1:b)))

(define-compound-unit/infer u3@
  (import)
  (export u2)
  (link (((u1 : s1^)) u1@)
        (((u2 : s1^)) u2@ u1)))

;(define-values/invoke-unit u3@ (import) (export s1^))
;
;a  ; 'aa
;b  ; '(b . bb)


;;; ======================================================================
;;;
;;; Mutual reference
;;;
;;; This doesn't work, because the mutual reference must be between function
;;; definitions, not constants.
;
;(define-signature a^
;  (a1 a2))
;
;(define-signature b^
;  (b1 b2))
;
;(define-unit a@
;  (import b^)
;  (export a^)
;  (init-depend b^)
;
;  (define a1 (cons b1 b1))
;  (define a2 'aa22))
;
;(define-unit b@
;  (import a^)
;  (export b^)
;
;  (define b1 'bb11)
;  (define b2 (cons a2 a2)))
;
;(define-compound-unit/infer c@
;  (import)
;  (export a^ b^)
;  (link b@ a@))
;
;(define-values/invoke-unit c@ (import) (export a^ b^))
;
;a1
;a2
;b1
;b2


;; ======================================================================
;;
;; Mutual reference
;; 

(define-signature a^
  (a))

(define-signature b^
  (b))

(define-unit a@
  (import b^)
  (export a^)
  (define (a x acc)
    (cond
      [(null? x) acc]
      [else (b (cdr x) (list* 'a (car x) acc))])))

(define-unit b@
  (import a^)
  (export b^)
  (define (b x acc)
    (cond
      [(null? x) acc]
      [else (a (cdr x) (list* 'b (car x) acc))])))

(define-compound-unit/infer ab@
  (import)
  (export a^ b^)
  (link a@ b@))

(define-values/invoke-unit ab@ (import) (export a^ b^))

(a '(1 2 3) '())

;; ======================================================================
;;
;; Importing a struct
;; 
;; Apparently it doesn't work:
;;
;;  struct-copy: not all accessors are statically known for structure type in: S

(define-signature struct^
  ((struct S (x y))))

(define-unit defstruct^
  (import)
  (export struct^)
  (struct S (x y)))

(define-unit test-struct@
  (import struct^)
  (export)

  (define s (S 100 200))
  (define t (struct-copy S s [x 300]))
  (list (S-x t) (S-y t)))

(invoke-unit test-struct@)
