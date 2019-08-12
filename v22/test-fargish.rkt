; test-fargish.rkt -- Unit tests for fargish.rkt, since (module+ test) doesn't
;                     seem to work with Typed Racket (in Racket 7.0)

#lang errortrace debug typed/racket
(require typed/debug/report)

(require "typed-wheel.rkt")
(require "types.rkt" "fargish.rkt" "model.rkt")
(require typed/rackunit phc-toolkit/typed-rackunit)

(module+ test
  (test-case "nodeclass inheritance"
    (define-spec spec
      (nodeclass (number [n : Integer])
        (value n))
      (nodeclass A
        (display-name 'this-is-A))
      (nodeclass B
        (is-a A))
      (tagclass C
        (is-a A)
        (display-name 'this-is-C))
      (nodeclass (D [x : Any])  ; Inherits being a tag. Should it not do that?
        (is-a C)))

    (define n (number 22))
    (check-equal? (hash-ref n 'class) 'number)
    (check-equal? (hash-ref n 'args) '(22))
    (check-equal? (hash-ref n 'value) 22)
    (check-equal? (hash-ref n 'tag? (const #f)) #f)

    (define a (A))
    (define b (B))
    (define c (C))
    (define d (D 'eks))

    (check-equal? (hash-ref a 'class) 'A)
    (check-equal? (hash-ref a 'args) '())
    (check-equal? (hash-ref a 'display-name) 'this-is-A)
    (check-equal? (hash-ref a 'value (const (void))) (void))
    (check-equal? (hash-ref a 'tag? (const #f)) #f)

    (check-equal? (hash-ref b 'class) 'B)
    (check-equal? (hash-ref b 'args) '())
    (check-equal? (hash-ref b 'display-name) 'this-is-A)
    (check-equal? (hash-ref b 'value (const (void))) (void))
    (check-equal? (hash-ref b 'tag? (const #f)) #f)

    (check-equal? (hash-ref c 'class) 'C)
    (check-equal? (hash-ref c 'args) '())
    (check-equal? (hash-ref c 'display-name) 'this-is-C)
    (check-equal? (hash-ref c 'value (const (void))) (void))
    (check-equal? (hash-ref c 'tag?) #t)

    (check-equal? (hash-ref d 'class) 'D)
    (check-equal? (hash-ref d 'args) '(eks))
    (check-equal? (hash-ref d 'display-name) 'this-is-C)
    (check-equal? (hash-ref d 'value (const (void))) (void))
    (check-equal? (hash-ref d 'tag?) #t))
  
  (test-case "make-tag"
    (define-spec spec
      (nodeclass A)
      (tagclass B
        (applies-to ([node])))
      (tagclass C
        (applies-to ([node1 (by-ports descriptee descriptor)]))))
    (define g (make-empty-graph spec))
    (let ([(g a) (make-node g (A))]
          [(g b) (make-tag g (B) a)]
          [(g c) (make-tag g (C) a)])
      (check-true (has-edge? g `((,b tagged) (,a tags))))
      (check-true (has-edge? g `((,c descriptee) (,a descriptor))))
      (check-true (has-tag? g 'B a))
      (check-true (has-tag? g 'C a))
      (check-false (has-tag? g 'A b))))

  (test-case "could-apply-to? and applies-to?"
    (define-spec spec
      (nodeclass (number [n : Integer])
        (value n)
        (display-name n))
      (nodeclass (letter [l : Char])
        (value l)
        (display-name l))
      (tagclass TODO
        (applies-to ([nodes (multiplicity any)])))
      (tagclass odd
        (applies-to ([node (of-class number) (multiplicity 1)])
          ; TODO The code that you have to write to specify a condition is
          ; pretty ugly, but it'll have to do for now. 24-Mar-2019
          (condition (let ([n (value-of g node)])
                       (and (not (void? n))
                            (odd? (cast n Integer))))))))

    (define g (make-empty-graph spec))
    (let ([(g n1) (make-node g (number 1))]
          [(g n2) (make-node g (number 2))]
          [(g l) (make-node g (letter #\a))])
      (check-true (could-apply-to? g 'odd n1))
      (check-false (could-apply-to? g 'odd l))
      (check-true (tagclass-applies-to? g 'odd n1))
      (check-false (tagclass-applies-to? g 'odd n2))
      (check-false (tagclass-applies-to? g 'odd l)))
  )
  )
