; wheel.rkt -- Wheels to not reinvent

#lang debug at-exp racket

(require (for-syntax racket/syntax) racket/syntax)

(provide (all-defined-out))

(define empty-set (set))
(define empty-hash (hash))

; cond with #:define
(define-syntax cond
  (syntax-rules (else =>)
    [(_)
     (raise-arguments-error 'cond "all clauses failed")]
    [(_ #:define name expr more ...)
     (let* ([name expr])
       (begin (cond more ...)))]
    [(_ [c => func] more ...)
     (let ([c-value c])
       (if c-value (func c-value) (cond more ...)))]
    [(_ [else body0 body ...])
     (begin body0 body ...)]
    [(_ [c body0 body ...] more ...)
     (if c (begin body0 body ...) (cond more ...))]))

(define (without-voids seq)
  (for/list ([x seq] #:when (not (void? x)))
    x))

(define (clamp lb ub x)
  (cond
    [(< x lb) lb]
    [(> x ub) ub]
    [else x]))

; Like the built-in take, but if lst has m < n elements, returns lst instead
; of throwing an exception.
; ECCH Won't override the standard take when (require "wheel.rkt").
;(define (take lst n)
;  (let loop ([l lst] [n n] [result '()])
;    (cond
;      [(null? l) lst]
;      [(<= n 0) (reverse result)] ;insanity
;      [else (loop (cdr l) (sub1 n) (cons (car l) result))])))

; Useful for unit tests involving inexact numbers
(define (trunc x)
  (if (number? x)
    (/ (truncate (* x 10000)) 10000)
    x))

; Useful for unit tests involving inexact numbers
(define (trunc-all x)
  (cond
    [(number? x)
     (trunc x)]
    [(hash? x)
     (for/hash ([(k v) x])
       (values (trunc-all k) (trunc-all v)))]
    [(pair? x)
     (map trunc-all x)]
    [(set? x)
     (for/set ([x* x])
       (trunc-all x*))]
    [else x]))

(define (->list x)
  (cond
    [(pair? x) x]
    [(hash? x) (hash->list x)]
    [(sequence? x) x]
    [else (list x)]))

(define (sorted xs)
  (sort (->list xs) string<? #:key ~a))

(define (sorted-by-cdr ht)
  (sort (->list ht) < #:key cdr))

(define (sorted-by-car ht)
  (sort (->list ht) < #:key car))

(define (values-sorted-by-key ht)
  (for/list ([k (sorted (hash-keys ht))])
    (hash-ref ht k)))

(define-syntax (define-singleton stx)
  (syntax-case stx ()
    [(define-singleton name)
     (with-syntax ([name? (format-id #'name "~a?" #'name
                                     #:source #'name #:props #'name)])
       #`(begin
           (define name
             (let ()
               (struct name () #:prefab)
               (name)))
           (define (name? x)
             (eq? name x))))]))

(define-syntax-rule (define-singletons name ...)
  (begin
    (define-singleton name) ...))

(define (hash-ref/sk ht key sk fk)
  (let ([value (hash-ref ht key (void))])
    (cond
      [(void? value) (if (procedure? fk) (fk) fk)]
      [else (sk value)])))

(define (hash-remove* h . keys)
  (for/fold ([h h])
            ([key keys])
    (hash-remove h key)))

(define (hash-map-values ht f)
  (for/hash ([(k v) ht])
    (values k (f v))))

(define (hash-by equiv-class xs)
  (for/fold ([ht empty-hash] #:result (hash-map-values ht reverse))
            ([x xs])
    (hash-update ht (equiv-class x) (λ (lst) (cons x lst)) '())))

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

(define (safe-take lst n)
  (cond
    [(null? lst)
     '()]
    [(zero? n)
     '()]
    [else (cons (car lst) (safe-take (cdr lst) (sub1 n)))]))

(define (safe-drop lst n)
  (cond
    [(null? lst)
     '()]
    [(zero? n)
     '()]
    [else (safe-drop (cdr lst) (sub1 n))]))

(define (safe-last x)
  (cond
    [(pair? x) (last x)]
    [else (void)]))

(define (safe-append . args)
  (cond
    [(null? args)
     '()]
    [(null? (cdr args))
     (->list (car args))]
    [else (append (->list (car args)) (safe-append (cdr args)))]))

(define (append-item lst . items)
  (append lst items))

; Allows any or all args to be void, and there need not be any args.
(define (safe-max . args)
  (for/fold ([result (void)])
            ([arg args])
    (cond
      [(void? arg) result]
      [(void? result) arg]
      [(<= arg result) result]
      [else arg])))

; Allows any or all args to be void, and there need not be any args.
(define (safe-min . args)
  (for/fold ([result (void)])
            ([arg args])
    (cond
      [(void? arg) result]
      [(void? result) arg]
      [(>= arg result) result]
      [else arg])))

; Allows any or all list elems to be void, and the list can be empty.
(define (safe-argmax proc lst)
  (for/fold ([result (void)] [m (void)] #:result result)
            ([elem lst])
    (if (void? elem)
      (values result m)
      (let* ([n (proc elem)])
        (cond
          [(void? n) (values result m)]
          [(void? result) (values elem n)]
          [(<= n m) (values result m)]
          [else (values elem n)])))))

; Allows any or all list elems to be void, and the list can be empty.
(define (safe-argmin proc lst)
  (for/fold ([result (void)] [m (void)] #:result result)
            ([elem lst])
    (if (void? elem)
      (values result m)
      (let* ([n (proc elem)])
        (cond
          [(void? n) (values result m)]
          [(void? result) (values elem n)]
          [(<= n m) (values result m)]
          [else (values elem n)])))))

(define (min-key . hts)
  (apply safe-min (all-hash-keys hts)))

(define (max-key . hts)
  (apply safe-max (all-hash-keys hts)))

(define (min-value . hts)
  (apply safe-min (all-hash-values hts)))

(define (max-value . hts)
  (apply safe-max (all-hash-values hts)))

(define (all-hash-keys hts)
  (for*/list ([ht hts]
              [k (hash-keys ht)])
    k))

(define (all-hash-values hts)
  (for*/list ([ht hts]
              [k (hash-values ht)])
    k))

(define-syntax-rule (first-value expr)
  (call-with-values (λ () expr)
    (λ (result . ignored) result)))

(define (exactly-one? pred seq)
  (= 1 (for/sum ([item seq])
         (if (pred item) 1 0))))

(define (eq?? x)
  (λ (x*) (eq? x x*)))

;; "Fills the void": if first argument is void, returns intersection of
;; remaining arguments.
;;
;; This is useful in for/set when you want the intersection of many sets:
;; (for/set ([st (void)]) (set-intersect* st ...)).  If you started by setting
;; st to an empty set and calling set-intersect, the result would always be an
;; empty set.
;TODO UT
(define (set-intersect* set-or-void . sets)
  (cond
    [(void? set-or-void)
     (if (null? sets)
       set-or-void
       (apply set-intersect* sets))]
    [(null? sets)
     set-or-void]
    [else (apply set-intersect set-or-void sets)]))


;;;based on version by soegaard, https://stackoverflow.com/a/45545280/1393162
;(define-match-expander dict
;  (λ (stx)
;     (syntax-case stx ()
;       [(_dict (
;; Doing this right looks very hard. The StackOverflow version doesn't call
;; dict-ref and consequently doesn't work for all values that support gen:dict.
;; It also doesn't match the whole dict analogously to hash-table.
