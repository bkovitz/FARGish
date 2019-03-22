; test-fargish.rkt -- Unit tests for fargish.rkt, since (module+ test) doesn't
;                     seem to work with Typed Racket (in Racket 7.0)

#lang errortrace typed/racket

(module+ test
  (require "fargish.rkt")
  (require typed/rackunit phc-toolkit/typed-rackunit)

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
    (check-equal? (hash-ref d 'tag?) #t)))
