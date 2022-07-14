(1 3 same)

(define (p1 canvas env)
  (canvas/paint canvas 3 (same (canvas/get 1))))

(let* ([source 1]
       [target 3]
       [func same])
  (define (p1 canvas env)
    (canvas/paint canvas target (func (canvas/get canvas source)))))
