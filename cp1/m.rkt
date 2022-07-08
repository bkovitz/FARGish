; m.rkt -- Can we get the simplest canvas-and-painters system to run in Racket?

#lang typed/racket

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

;(: canvas/get-value (-> Canvas Index (U Value Fizzle)))
;(define (canvas/get-value c i)
;  (
