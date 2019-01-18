; Experiments: parsing desiderata

#lang debug at-exp racket

(require "wheel.rkt")
(require expect/rackunit (only-in rackunit test-case))

(define (desideratum->items desideratum)
  (match desideratum
    [`(find (,items ...) ,body ...)
      items]
    [`(find/build (,items ...) ,body ...)
      items]
    [else (raise-arguments-error 'desideratum->items
            @~a{Invalid desideratum: @desideratum})]))


;; ======================================================================
;;
;; Unit tests
;;

(module+ test
  (define d-eqn '(find ([eqn (of-class 'equation)])
                   (splice-into 'ws eqn)))

  (define d-spl
    `(find/build ([9′ (in-ctx 'ws)]
                  [4′ (in-ctx 'ws)]
                  [5′ (in-ctx 'ws)]
                  [+′ (in-ctx 'ws)])
       (bind 9′ 9)
       (bind 4′ 4)
       (bind 5′ 5)
       (bind +′ +)))

  (test-case "desideratum->items"
    (check-equal? (desideratum->items d-eqn)
                  '([eqn (of-class 'equation)]))
    (check-equal? (desideratum->items d-spl)
                  '([9′ (in-ctx 'ws)]
                    [4′ (in-ctx 'ws)]
                    [5′ (in-ctx 'ws)]
                    [+′ (in-ctx 'ws)])))
  )

;;;;;; REPL code

(define d-eqn '(find ([eqn (of-class 'equation)])
                 (splice-into 'ws eqn)))

(define d-spl
  `(find/build ([9′ (in-ctx 'ws)]
                [4′ (in-ctx 'ws)]
                [5′ (in-ctx 'ws)]
                [+′ (in-ctx 'ws)])
     (bind 9′ 9)
     (bind 4′ 4)
     (bind 5′ 5)
     (bind +′ +)))
