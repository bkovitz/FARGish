; typed-wheel.rkt -- Wheels to not reinvent, version for Typed Racket

#lang debug at-exp typed/racket

(require typed-struct-props racket/unsafe/ops)
(require racket/syntax syntax/parse syntax/parse/define syntax/free-vars
         (for-syntax racket/syntax syntax/parse syntax/free-vars))

(provide (all-defined-out))

(define-syntax-rule (first-value expr)
  (call-with-values (λ () expr)
    (λ (result . ignored) result)))

; Is it possible to forward arguments like this in Typed Tracket?
;(: first-value/ (All (dom ...)
;                  (All (rng0 rng ...)
;                    (-> (->* (dom ...) (Values rng0 rng ...)) rng0))))
;(define (first-value/ f)
;  (λ args
;    (call-with-values (λ () (apply f args))
;      (λ ([result : rng0] . ignored) result))))


;; ======================================================================
;;
;; Generic UnorderedPair
;;

(struct/props (A)
  UnorderedPair ([a : A] [b : A])
                #:transparent
                #:property prop:equal+hash
                           (list upair=? upair-hash1 upair-hash2))

(: upair=? (All (A B)
          (-> (UnorderedPair A)
              (UnorderedPair B)
              (-> Any Any Boolean)
              Any)))
(define (upair=? up1 up2 recursive=?)
  (let ([a1 (UnorderedPair-a up1)]
        [a2 (UnorderedPair-a up2)]
        [b1 (UnorderedPair-b up1)]
        [b2 (UnorderedPair-b up2)])
    (or (and (equal? a1 a2) (equal? b1 b2))
        (and (equal? a1 b2) (equal? b1 a2)))))

(: upair-hash1 (All (A) (-> (UnorderedPair A) (-> Any Fixnum) Fixnum)))
(define (upair-hash1 up recursive-equal-hash)
  (let ([a (UnorderedPair-a up)]
        [b (UnorderedPair-b up)])
    (unsafe-fxxor (equal-hash-code a) (equal-hash-code b))))

(: upair-hash2 (All (A) (-> (UnorderedPair A) (-> Any Fixnum) Fixnum)))
(define (upair-hash2 up recursive-equal-hash)
  (let ([a (UnorderedPair-a up)]
        [b (UnorderedPair-b up)])
    (unsafe-fxxor (equal-secondary-hash-code a) (equal-secondary-hash-code b))))
