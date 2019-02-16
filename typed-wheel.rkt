; typed-wheel.rkt -- Wheels to not reinvent, version for Typed Racket

#lang debug at-exp typed/racket

(require typed-struct-props racket/unsafe/ops)
(require racket/syntax syntax/parse syntax/parse/define syntax/free-vars
         (for-syntax racket/syntax syntax/parse syntax/free-vars))

(provide (all-defined-out))

(define-type (Hashof K V) (Immutable-HashTable K V))

; cond with #:define
(define-syntax cond
  (syntax-rules (else =>)
    [(_)
     (raise-arguments-error 'cond "all clauses failed")]
    [(_ #:define name expr more ...)
     (let* ([name expr])
       (begin (cond more ...)))]
    [(_ #:match-define pat expr more ...)
     (match-let ([pat expr])
       (begin (cond more ...)))]
    [(_ [c => func] more ...)
     (let ([c-value c])
       (if c-value (func c-value) (cond more ...)))]
    [(_ [else body0 body ...])
     (let () body0 body ...)]
    [(_ [c body0 body ...] more ...)
     (if c (let () body0 body ...) (cond more ...))]))

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
;; Currying
;;

(: curry+ : Number * -> (-> Number * Number))
(define (curry+ . args)
  (let ([sum (apply + args)])
    (λ args (apply + sum args))))

;; ======================================================================
;;
;; Generic UnorderedPair
;;

(struct/props (A)
  UnorderedPair ([a : A] [b : A])
                #:transparent
                #:property prop:equal+hash
                           (list upair=? upair-hash1 upair-hash2))

(: upair=? (All (A B) (-> (UnorderedPair A)
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
    (unsafe-fx+ (equal-hash-code a) (equal-hash-code b))))

(: upair-hash2 (All (A) (-> (UnorderedPair A) (-> Any Fixnum) Fixnum)))
(define (upair-hash2 up recursive-equal-hash)
  (let ([a (UnorderedPair-a up)]
        [b (UnorderedPair-b up)])
    (unsafe-fx+ (equal-secondary-hash-code a) (equal-secondary-hash-code b))))

(: upair->list (All (A) (-> (UnorderedPair A) (List A A))))
(define (upair->list up)
  (list (UnorderedPair-a up) (UnorderedPair-b up)))

(: list->upair (All (A) (-> (List A A) (UnorderedPair A))))
(define (list->upair lst)
  (UnorderedPair (car lst) (cadr lst)))

;; ======================================================================
;;
;; Collection utilities
;;

(: hash-merge (All (K V) (-> (Hashof K V) * (Hashof K V))))
(define (hash-merge . hts)
  (cond
    [(null? hts) ((inst hash K V))]
    #:define h0 (car hts)
    [(null? (cdr hts)) h0]
    #:define h1 (cadr hts)
    [else
      (let loop ([h0 : (Hashof K V) h0]
                 [pos (hash-iterate-first h1)])
        (cond
          [(not pos) (apply hash-merge h0 (cddr hts))]
          [else (let-values ([(k v) (hash-iterate-key+value h1 pos)])
                  (loop (hash-set h0 k v)
                        (hash-iterate-next h1 pos)))]))]))
