; xequal.rkt -- Experiments with typed-struct-props for prop:equal+hash

#lang debug at-exp typed/racket

(require typed-struct-props)

(struct/props (A) UnorderedPair ([a : A] [b : A])
                                #:transparent
                                #:property prop:equal+hash
                                           (list up=? up-hash1 up-hash2))

(: up=? (All (A B)
          (-> (UnorderedPair A)
              (UnorderedPair B)
              (-> Any Any Boolean)
              Any)))
(define (up=? up1 up2 recursive=?)
  (let ([a1 (UnorderedPair-a up1)]
        [a2 (UnorderedPair-a up2)]
        [b1 (UnorderedPair-b up1)]
        [b2 (UnorderedPair-b up2)])
    (or (and (equal? a1 a2) (equal? b1 b2))
        (and (equal? a1 b2) (equal? b1 a2)))))

(: up-hash1 (All (A) (-> (UnorderedPair A) (-> Any Fixnum) Fixnum)))
(define (up-hash1 up recursive-equal-hash)
  (let ([a (UnorderedPair-a up)]
        [b (UnorderedPair-b up)])
    (bitwise-xor (equal-hash-code a) (equal-hash-code b))))

(: up-hash2 (All (A) (-> (UnorderedPair A) (-> Any Fixnum) Fixnum)))
(define (up-hash2 up recursive-equal-hash)
  (let ([a (UnorderedPair-a up)]
        [b (UnorderedPair-b up)])
    (bitwise-xor (equal-secondary-hash-code a) (equal-secondary-hash-code b))))

(define-type E (UnorderedPair Symbol))

(define a : E (UnorderedPair 'a 'b))
(define b : E (UnorderedPair 'x 'y))
(define c : E (UnorderedPair 'a 'b))
(define d : E (UnorderedPair 'y 'x))
