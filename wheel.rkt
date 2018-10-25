; wheel.rkt -- Wheels to not reinvent

#lang debug at-exp racket

(provide (all-defined-out))

(define empty-set (set))
(define empty-hash (hash))

(define (hash-ref/sk ht key sk fk)
  (let ([value (hash-ref ht key (void))])
    (cond
      [(void? value) (if (procedure? fk) (fk) fk)]
      [else (sk value)])))

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

(define (exactly-one? pred seq)
  (= 1 (for/sum ([item seq])
         (if (pred item) 1 0))))

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
