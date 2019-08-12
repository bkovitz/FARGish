; Experiments to see how fast different, comparable constructs run in Racket

#lang debug at-expr racket

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

;(test 200000)

; for/list appears to do a reverse: it takes about 150% as long as for/fold,
; the difference being almost entirely spent on garbage collection.
; for/pvector is very slow: around 70x slower than for/fold.

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

;(testd 200000 2)

; Wow, dict-ref's performance is horrible: 15x slower than hash-ref.
; assoc outperforms hash-ref only when the alist has 2 or fewer elements.

