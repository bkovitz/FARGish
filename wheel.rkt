; wheel.rkt -- Wheels to not reinvent

#lang debug at-exp racket

(require racket/syntax syntax/parse syntax/parse/define syntax/free-vars
         (for-syntax racket/syntax syntax/parse syntax/free-vars))
(require racket/hash sugar)
(require expect/rackunit (only-in rackunit test-case))

(provide (all-defined-out))

(define empty-set (set))
(define empty-hash (hash))
(define empty-hasheq (hasheq))

; 'let' that works like let* and let*-values depending on whether you put
; parentheses around the variables to bind to. The same form allows named
; 'let', though without multiple values. You must put something in the body.
; TODO Make this work with Typed Racket.
(define-syntax (let stx)
  (syntax-parse stx
    [(_ () body:expr ...+)
     #'(begin body ...)]
    [(_ ([name:id e:expr] more ...) body:expr ...+)
     #'((λ (name) (let (more ...) body ...)) e)]
    [(_ ([(name:id ...+) e:expr] more ...) body:expr ...+)
     #'(call-with-values (λ () e) (λ (name ...) (let (more ...) body ...)))]
    [(_ letname:id ([name:id e:expr] ...) body:expr ...+)
     #'(letrec ([letname (λ (name ...) body ...)]) (letname e ...))]))

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
    [(_ #:do expr more ...)
     (begin expr (cond more ...))]
    [(_ [c => func] more ...)
     (let ([c-value c])
       (if c-value (func c-value) (cond more ...)))]
    [(_ [else body0 body ...])
     (let () body0 body ...)]
    [(_ [c body0 body ...] more ...)
     (if c (let () body0 body ...) (cond more ...))]))

(define-syntax ~>
  (syntax-rules ()
    [(_ x) x]
    [(_ x (f args ...) more ...)
     (~> (f x args ...) more ...)]
    [(_ x f more ...)
     (~> (f x) more ...)]))

(define-syntax ~>>
  (syntax-rules ()
    [(_ x) x]
    [(_ x (f args ...) more ...)
     (~>> (f args ... x) more ...)]
    [(_ x f more ...)
     (~>> (f x) more ...)]))

; Useful in (map appl list-of-funcs list-of-args)
(define (appl f . args) (apply f args))

; Useful for SACC?
; MAYBE Support procedures with variable arity. This might run slowly, though.
(define (apply/curried f . args)
  (let*-values ([(f-args more-args) (split-at args (procedure-arity f))]
                [(g) (apply f f-args)])
    (cond
      [(null? more-args) g]
      [else (apply apply/curried g more-args)])))
; MAYBE Make a define/curried macro that does the above automatically so
; that the caller doesn't need to care.

; Quick curries

(define (not/ f)
  (compose1 not f))

(define (cons/ a)
  (λ (d)
    (cons a d)))

(define (set->pred st)
  (curry set-member? st))

(define (hash->pred ht)
  (curry hash-has-key? ht))

(define (apply-to/ . args)
  (λ (f)
    (apply f args)))

(define (set-add/ . args)
  (λ (st)
    (for/fold ([st st])
              ([arg args])
      (set-add st arg))))

(define (without-voids seq)
  (for/list ([x seq] #:when (not (void? x)))
    x))

(define (set-add* st . xs)
  (for/fold ([st st])
            ([x xs])
    (set-add st x)))

(define (.. lb ub [step 1])
  (range lb (add1 ub) step))

(define (clamp lb ub x)
  (cond
    [(< x lb) lb]
    [(> x ub) ub]
    [else x]))

(define unit-clamp (curry clamp 0.0 1.0))

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
(define (round/ut x)
  (if (number? x)
    (/ (round (* x 10000.0)) 10000.0)
    x))

; Useful for unit tests involving inexact numbers
(define (round-all/ut x)
  (cond
    [(number? x)
     (round/ut x)]
    [(hash? x)
     (for/hash ([(k v) x])
       (values (round-all/ut k) (round-all/ut v)))]
    [(pair? x)
     (map round-all/ut x)]
    [(set? x)
     (for/set ([x* x])
       (round-all/ut x*))]
    [else x]))

;Rendered obsolete by sugar
#;(define (->list x)
  (cond
    [(pair? x) x]
    [(hash? x) (hash->list x)]
    [(sequence? x) x]
    [else (list x)]))

(define (->set x)
  (cond
    [(set? x) x]
    [(list? x) (list->set x)]
    [(sequence? x) (for/set ([x* x]) x)]
    [else (raise-arguments-error '->set
            @~a{Can't convert @x to set.})]))

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

(define (hash-set-set ht k1 k2 v)
  (hash-ref/sk ht k1
    (λ (ht2) ;sk
      (hash-set ht k1 (hash-set ht2 k2 v)))
    (λ () ;fk
      (hash-set ht k1 (hash k2 v)))))

(define (index-by f xs)
  (for/hash ([x xs])
    (values (f x) x)))

(define (index-by/eq f xs)
  (for/hasheq ([x xs])
    (values (f x) x)))

(define hash->f 
  (case-lambda
    [(ht)
     (λ (k) (hash-ref ht k))]
    [(ht default)
     (λ (k) (hash-ref ht k default))]))

(define (hash->numeric-f ht)
  (hash->f ht 0.0))

(define (hash->numeric-f/arity-2 ht)
  (λ (k1 k2) (hash-ref ht (list k1 k2) 0.0)))

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

(define (hasheq-by equiv-class xs)
  (for/fold ([ht empty-hasheq] #:result (hash-map-values ht reverse))
            ([x xs])
    (hash-update ht (equiv-class x) (cons/ x) '())))

(define (hasheq-by/set ->key ->value xs)
  (for/fold ([ht empty-hasheq])
            ([x xs])
    (hash-update ht (->key x) (set-add/ (->value x)) empty-set)))

(define (zip-hash ks vs)
  (for/hash ([k ks] [v vs])
    (values k v)))

; Keys in hashes to the right override keys in hashes to the left.
(define hash-merge
  (case-lambda
    [() empty-hash]
    [(h0 . hs)
     (apply hash-union h0 hs #:combine/key (λ (k v0 v) v))]))

; Multiplication, but "fills in" void as 1.0 if possible.
(define (safe-* . args)
  (let* ([args (filter-not void? args)])
    (cond
      [(null? args) (void)]
      [else (apply * args)])))

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

(define (safe-eqv? . args)
  (cond
    [(ormap void? args) #f]
    [else (apply eqv? args)]))

(define (safe->? . args)
  (let ([args (without-voids args)])
    (cond
      [(null? args) #f]
      [(null? (cdr args)) #f]
      [else (apply > args)])))

(define (safe-integer->string n)
  (cond
    [(void? n) (void)]
    [(integer? n) (number->string n)]
    [else (void)]))

(define (safe-string-length s)
  (cond
    [(void? s) (void)]
    [else (string-length s)]))

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
      [else (max arg result)])))

; Allows any or all args to be void, and there need not be any args.
(define (safe-min . args)
  (for/fold ([result (void)])
            ([arg args])
    (cond
      [(void? arg) result]
      [(void? result) arg]
      [else (min arg result)])))

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

(define (absdiff x y)
  (cond
    [(void? x) x]
    [(void? y) y]
    [else (abs (- x y))]))

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

#;(define (eq?? x)
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

; Is x a quoted thing?
(define (quoted? x)
  (cond
    [(null? x) #f]
    [(not (pair? x)) #f]
    [else (eq? 'quote (car x))]))

; Removes quoted? elements from all levels of ls. However, if ls is itself
; quoted, that will have no effect.
(define (remove-quoted ls)
  (cond
    [(null? ls) ls]
    #:define a (car ls)
    #:define d (cdr ls)
    [(quoted? a) (remove-quoted d)]
    [(atom? a) (cons a (remove-quoted d))]
    [else (cons (remove-quoted a) (remove-quoted d))]))

(module+ test
  (test-case "remove-quoted"
    (check-equal? (remove-quoted '()) '())
    (check-equal? (remove-quoted '(a b c)) '(a b c))
    (check-equal? (remove-quoted '('a b c)) '(b c))
    (check-equal? (remove-quoted '((a 'b) c)) '((a) c))
    (check-equal? (remove-quoted '('(a b) c)) '(c))
    (check-equal? (remove-quoted '(splice-into 'ws eqn)) '(splice-into eqn))
))

(define log+1
  (case-lambda
    [(x) (log (+ 1.0 x))]
    [(x b) (log (+ 1.0 x) b)]))

;;;based on version by soegaard, https://stackoverflow.com/a/45545280/1393162
;(define-match-expander dict
;  (λ (stx)
;     (syntax-case stx ()
;       [(_dict (
;; Doing this right looks very hard. The StackOverflow version doesn't call
;; dict-ref and consequently doesn't work for all values that support gen:dict.
;; It also doesn't match the whole dict analogously to hash-table.

;; ======================================================================
;;
;; Transparent closure
;;
;; cλ returns a closure that you can inspect at run-time.

(struct closure* (f ht/free-vars body)
                 #:transparent
                 #:property prop:procedure 0)

(define (closure-value c name)
  (hash-ref (closure*-ht/free-vars c) name (void)))
(define (closure->free-vars c)
  (hash-keys (closure*-ht/free-vars c)))
(define closure->body closure*-body)

(define-syntax (cλ stx)
  (syntax-parse stx
    [(_ args body0 body ...)
     (define expanded-lam (local-expand #'(lambda args body0 body ...)
                                         'expression
                                         '()))
     (syntax-parse expanded-lam
       #:literals (#%plain-lambda)
       [(~and whole (#%plain-lambda (arg ...) body))
        (with-syntax ([(fv ...) (free-vars #'whole)])
          #`(closure* #,expanded-lam
                      (hash (~@ 'fv fv) ...)
                      '#,stx))])]))

(module+ test
  (test-case "cλ"
    (define (make-transparent-closure x)
      (cλ (a) (list a x)))
    (define f (make-transparent-closure 'xxx))
    (check-equal? (f 'aaa) '(aaa xxx))
    (check-equal? (list->set (closure->free-vars f)) (set 'x))
    (check-equal? (closure-value f 'x) 'xxx)
    (check-equal? (closure-value f 'undefined) (void))
    (check-equal? (closure->body f) '(cλ (a) (list a x)))))
