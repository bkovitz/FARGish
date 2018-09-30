; Throwaway code for learning something about Racket

#lang debug at-exp racket

;(define (commutative-operands=? co1 co2 re=?)
;  (define xs1 (commutative-operands-xs co1))
;  (define xs2 (commutative-operands-xs co2))
;  (and (= (length xs1) (length xs2))
;       (re=? (apply set xs1) (apply set xs2))))
;
;(define (commutative-operands-hash-1 co rhc)
;  (define xs (commutative-operands-xs co))
;  (rhc (cons (length xs) (apply set xs))))
;
;(define (commutative-operands-hash-2 co rhc)
;  (define xs (commutative-operands-xs co))
;  (rhc (cons (length xs) (apply set xs))))
;
;(struct commutative-operands (xs) #:transparent
;  #:methods gen:equal+hash
;  [(define equal-proc commutative-operands=?)
;   (define hash-proc commutative-operands-hash-1)
;   (define hash2-proc commutative-operands-hash-2)])
;
;(define p (for*/set ([i (in-range 1 4)]
;                     [j (in-range 1 4)])
;            (commutative-operands (list i j))))
;
;p
;
;(define c1 (commutative-operands '(2 3)))
;(define c2 (commutative-operands '(3 2)))
;(define x1 (commutative-operands-xs c1))
;(define x2 (commutative-operands-xs c2))
;
;(equal-hash-code c1)
;(equal-hash-code c2)
;(equal-hash-code x1)
;(equal-hash-code x2)
;
;(define ns (make-base-namespace))
;(define operand-pairs (for*/set ([i (in-range 1 13)]
;                                 [j (in-range 1 13)])
;                        `(,i ,j)))
;(define tuples (for*/list ([ij operand-pairs]
;                           [op '(+ - *)])
;                 (match-define `(,i ,j) ij)
;                 (define expr `(,op ,i ,j))
;                 (define result (eval expr ns))
;                 `(,i ,op ,j ,result)))

(require data/pvector)

(define (test n)
  (time
    (for/list ([x (in-range n)])
      (list x x)))
  (time
    (for/fold ([result '()])
              ([x (in-range n)])
      (cons (list x x) result)))
  (time
    (for/pvector ([x (in-range n)])
      (list x x)))
  (void))

;(test 500000)

(define (testd iters n)
  (define alist (for/list ([i n])
                  (cons i 'a)))
  (define ht (make-immutable-hash alist))
  (time
    (for/fold ([result '()])
              ([_ iters])
      (cons (hash-ref ht 0) (hash-ref ht (sub1 n)))))
  (time
    (for/fold ([result '()])
              ([_ iters])
      (cons (dict-ref ht 0) (dict-ref ht (sub1 n)))))
  (time
    (for/fold ([result '()])
              ([_ iters])
      (cons (dict-ref alist 0) (dict-ref alist (sub1 n)))))
  (time
    (for/fold ([result '()])
              ([_ iters])
      (cons (cdr (assoc 0 alist)) (cdr (assoc (sub1 n) alist))))))

; Wow, dict-ref's performance is horrible: 15x slower than hash-ref.
; assoc outperforms hash-ref only when the alist has 2 or fewer elements.

