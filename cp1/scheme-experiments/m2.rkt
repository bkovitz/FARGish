; m.rkt -- Can we get the simplest canvas-and-painters system to run in Racket?

#lang typed/racket

(define *max-clarity* 6)

(struct Det-Addr ([container : Container] [index : Index]) #:transparent)

(define-type Container-Ref (U 'working-soup 'main-canvas))

(define-type Container (U Canvas Soup))

(struct Canvas ([s : String] [clarities : (Mutable-Vectorof Integer)])
; characters, and a clarity for each character
        #:transparent)

(define-type Soup (Setof Painter))

(define-type Index Integer)

(define-type Painter (List Addr Addr Func))

;(define-type Func (U (-> Value (U Value Fizzle)) Value Painter-Template))
(define-type Func (-> Char Char))

(define-type Value (U Integer Char Painter Func))

(define-type Addr (U Index Soup Container-Ref Painter-Template Det-Addr))

(define-type Painter-Template (List Addr-Expr Addr-Expr Func))

;(define-type Addr-Expr (U Addr))  ; TODO (+ 'I 1), etc.
(define-type Addr-Expr (U Symbol Integer (List* '+ (Listof Addr-Expr))))

(struct Env ([canvas : Canvas] [hash : (Immutable-HashTable Any Any)])
        #:transparent)

(struct Fizzle () #:transparent)


(: string->canvas (-> String Canvas))
(define (string->canvas s)
  (Canvas (string-append s)
          (apply vector (map (λ ([c : Char])
                               (if (eq? c #\space) 0 4))
                             (string->list s)))))

(define c (string->canvas "      "))

(define d (Det-Addr c 2))

(define e (Env c (hash)))

(: succ Func)
(define (succ x)
  (cond
    [(exact-integer? x) (add1 x)]
    [else (raise (Fizzle))]))

;(: fizzle (-> Nothing))
;(define (fizzle)
;  (raise (Fizzle)))

(: p Painter)
(define p `(1 3 ,succ))


(: run-painter (-> Env Painter Env))
(define (run-painter env painter)
  (match painter
    [(list source target func)
     #:when (and (exact-integer? source) (exact-integer? target) (procedure? func))
     (paint/env env target (func (at-addr/env env source)))]))

(: paint/env (-> Env Index Char Env))
(define (paint/env env i value)
  ;(Env (paint/canvas (Env-canvas env) i value) (Env-hash env)))
  (struct-copy Env env [canvas (Env-canvas env)]))

(: at-addr/env (-> Env Index Char))
(define (at-addr/env env i)
  (at-addr/canvas (Env-canvas env) i))

(: at-addr/canvas (-> Canvas Index Char))
(define (at-addr/canvas c i)
  (string-ref (Canvas-s c) i))

