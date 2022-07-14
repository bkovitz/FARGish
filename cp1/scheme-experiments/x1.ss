(define (qu x)
  (list `quote x))

(define (f painter)
  (let-values ([(a b c) (apply values painter)])
    (display (list c b a))))

(f '(x y z))
