; typed-wheel.rkt -- Wheels to not reinvent, version for Typed Racket

#lang debug at-exp typed/racket

(require typed-struct-props racket/unsafe/ops)
(require/typed mischief/sort
   [topological-sort (All (A)
                       (->* [(Listof A) (-> A (Listof A))]
                            (#:cycle (-> (Listof A) Nothing))
                            (Listof A)))])
(require racket/syntax syntax/parse syntax/parse/define syntax/free-vars
         (for-syntax racket/syntax syntax/parse syntax/free-vars))
(require (only-in typed/racket [let let/TR]))

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
    [(_ #:do expr more ...) ;UNTESTED
     (begin expr (cond more ...))]
    [(_ [c => func] more ...)
     (let ([c-value c])
       (if c-value (func c-value) (cond more ...)))]
    [(_ [else body0 body ...])
     (let () body0 body ...)]
    [(_ [c body0 body ...] more ...)
     (if c (let () body0 body ...) (cond more ...))]))

; 'let' that works like let* and let*-values depending on whether you put
; parentheses around the variables to bind to. The same form allows named
; 'let', though without multiple values. You must put something in the body.
(define-syntax (let stx)
  (define-syntax-class binding
    #:datum-literals [:]
    #:attributes [lhs rhs]
    ; Returns lhs in form suitable for splicing into a let*-values form.
    (pattern [name:id rhs:expr]   ; (let ([a 'x] ....
             #:with lhs #'(name))
    (pattern [name:id : type:expr rhs:expr]  ; (let ([a : Symbol 'x] ....
             #:with lhs #'([name : type]))
    (pattern [(name:id ...) rhs:expr]        ; (let ([(a b) (values 'x 2)] ....
             #:with lhs #'(name ...))
    (pattern [([name:id : type:expr] ...) rhs:expr]
                ; (let ([([a : Symbol] [b : Integer]) (values 'x 2)] ....
             #:with lhs #'([name : type] ...)))

  (syntax-parse stx
    #:datum-literals [:]
    [(_ (b:binding ...) body:expr ...+)
     #'(let*-values ([(~@ b.lhs) b.rhs] ...) body ...)]
    [(_ whatever ...)
     #'(let/TR whatever ...)]))

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
;; Wrapper functions that accept void
;;

(: safe-string-length : (U String Void) -> (U Integer Void))
(define (safe-string-length s)
  (cond
    [(void? s) (void)]
    [else (string-length s)]))

(: safe-integer->string : (U Number Void) -> (U String Void))
(define (safe-integer->string n)
  (cond
    [(void? n) (void)]
    [(integer? n) (number->string n)]
    [else (void)]))

(: safe-eqv? : Any Any -> Boolean)
(define (safe-eqv? x1 x2)
  (cond
    [(or (void? x1) (void? x2))
     #f]
    [else (eqv? x1 x2)]))

;; ======================================================================
;;
;; Currying
;;

(: not/ : (All (a ...) (-> a ... a Any) -> (-> a ... a Boolean)))
(define (not/ f)
  (λ args (not (apply f args))))

(: curry+ : Number * -> (-> Number * Number))
(define (curry+ . args)
  (let ([sum (apply + args)])
    (λ args (apply + sum args))))

(: set->pred (All (A) (Setof A) -> (-> Any Boolean)))
(define (set->pred st)
  (λ (x) (set-member? st x)))

(: hash->pred (All (K V) (Hashof K V) -> (-> K Boolean)))
(define (hash->pred ht)
  (curry hash-has-key? ht))

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

(: set-add* (All (A) (-> (Setof A) A * (Setof A))))
(define (set-add* st . xs)
  (for/fold ([st st])
            ([x xs])
    (set-add st x)))

(: hash-remove* (All (K V) (Hashof K V) K * -> (Hashof K V)))
(define (hash-remove* h . keys)
  (for/fold ([h : (Hashof K V) h])
            ([key keys])
    (hash-remove h key)))

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

(: hash->f : (All (K V D) (case->
                            [-> (Hashof K V) (-> K V)]
                            [-> (Hashof K V) D (-> K (U V D))])))
(define hash->f
  (case-lambda
    [(ht)
     (λ (k) (hash-ref ht k))]
    [(ht default)
     (λ (k) (hash-ref ht k (const default)))]))

;(: ->list : (All (A) (U A (Listof A) (Setof A)) -> (Listof A)))
(define-syntax-rule (->list x)
  (cond
    [(list? x) x]
    [(set? x) (set->list x)]
    [else (list x)]))

(: first-non-void (All (a) (Listof (U a Void)) -> (U a Void)))
(define (first-non-void ls)
  (cond
    [(null? ls) (void)]
    #:define a (car ls)
    [(void? a) (first-non-void (cdr ls))]
    [else a]))

(: non-voids (All (a) (U (Listof (U a Void)) Void) -> (Listof a)))
(define (non-voids ls)
  (cond
    [(void? ls) '()]
    [else (for/list ([x ls]
                     #:when (not (void? x))) : (Listof a)
            x)]))

    ;[else (filter (not/ void?) ls)]))

;; ======================================================================
;;
;; Inheritance
;;

(: merge-from-ancestors (All (A) A (-> A (Listof A)) (-> A A A) -> A))
(define (merge-from-ancestors item item->parents parent+item->item)
  (let ([ancestor-seq (topological-sort (list item) item->parents)])
    (cond
      [(null? ancestor-seq) item]
      [else (let loop ([parent (car ancestor-seq)]
                       [ancestor-seq (cdr ancestor-seq)])
              (cond
                [(null? ancestor-seq) parent]
                [else (loop (parent+item->item parent (car ancestor-seq))
                            (cdr ancestor-seq))]))])))

;; ======================================================================
;;
;; Randomness
;;

;TODO Remove this or replace it with a wrapper around discrete-dist.
(: weighted-choice-by (All (A) (-> A Real) (Listof A) -> (U Void A)))
(define (weighted-choice-by f choices)
  (define-values (choice-pairs total) (make-choice-pairs f choices))
  (cond
    [(null? choice-pairs)
     (void)]
    [(null? (cdr choice-pairs))
     (cdar choice-pairs)]
    [else (let ([r (* (random) total)])
            (let loop ([choice-pairs choice-pairs])
              (let ([pair (car choice-pairs)])
                (cond
                  [(>= r (car pair))
                   (cdr pair)]
                  [else (loop (cdr choice-pairs))]))))]))
  
(: make-choice-pairs (All (A)
                       (-> A Real) (Listof A) -> (Values (Listof (Pair Real A))
                                                         Real)))
(define (make-choice-pairs f choices)
  (for/fold ([pairs : (Listof (Pair Real A)) '()] [w 0.0])
            ([choice choices])
        (let ([delta (f choice)])
          (if (zero? delta)
            (values pairs w)
            (values (cons `(,w . ,choice) pairs)
                    (+ delta w))))))

;(displayln "TYPED-WHEEL") ;WHY does this get printed 5 or more times when I
;recompile test-fargish.rkt in DrRacket?

;; ======================================================================
;;
;; JSON
;;

(: ->jsexpr : Any -> JSExpr)
(define (->jsexpr x)
  (cond
    [(boolean? x) x]
    [(string? x) x]
    [(exact-integer? x) x]
    [(and (inexact-real? x) (rational? x)) x]
    [(list? x) (map ->jsexpr x)]
    [(hash? x) (for/hash ([(k v) (in-hash x)]) : (Hashof Symbol JSExpr)
                 (values (->symbol k) (->jsexpr v)))]
    [(void? x) 'null]
    [else (format "~a" x)]))

