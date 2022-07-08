; m.rkt -- Can we get the simplest canvas-and-painters system to run in Racket?

#lang typed/racket

(define *max-clarity* 6)

(struct Det-Addr ([container : Container] [index : Index]) #:transparent)

(define-type Container (U Canvas Soup))

(struct Canvas ([s : String] [clarities : (Mutable-Vectorof Integer)])
; characters, and a clarity for each character
        #:transparent)

(define-type Soup (Setof Painter))

(define-type Index Integer)

(define-type Painter (List Addr Addr Func))

(define-type Func (-> Value Value))

(define-type Value (U Integer Char Painter Func))

(define-type Addr (U Index Soup Painter-Template Det-Addr))

(define-type Painter-Template (List Addr-Expr Addr-Expr Func))

(define-type Addr-Expr (U Addr))  ; TODO (+ 'I 1), etc.

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

; The default (simplest) way to map an Addr to a Det-addr.
;(: eval-as/det-addr/default (-> Env (U Value Addr) (U Det-Addr Fizzle)))
;(define (eval-as/det-addr/default env x)
(define (eval-as/det-addr/default [env : Env] [x : (U Value Addr)])
  : (U Det-Addr Fizzle)
  (cond
    [(Det-Addr? x) x]
    [else (Fizzle)]))

(: canvas/paint (-> Canvas Index Char Void))
(define (canvas/paint c i v)
  ; Paints character v on canvas c at index i.
  (let* ([s (Canvas-s c)]
         [clarities (Canvas-clarities c)]
         [clarity (vector-ref clarities i)])
    (cond
      [(eq? v #\space)
       (when (> clarity 0)
             (vector-set! clarities i (sub1 clarity)))]
      [(= clarity 0)
       (begin
         (string-set! s i v)
         (vector-set! clarities i 1))]
      [(not (eq? v (string-ref s i)))
       (vector-set! clarities i (sub1 clarity))]
      [else ; (eq? (string-ref s i) v)
       (when (< clarity *max-clarity*)
             (vector-set! clarities i (add1 clarity)))])))

(: canvas/get-value (-> Canvas Index (U Char Fizzle)))
(define (canvas/get-value c i)
  (string-ref (Canvas-s c) i))
