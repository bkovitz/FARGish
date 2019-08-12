; equations.rkt -- Code to make a set of equations and put them into the
;                  slipnet so they can be searched by spreading activation

#lang debug at-exp racket

(require (prefix-in m: "model1.rkt")
         (prefix-in sl: "make-slipnet.rkt")
         (prefix-in e:
           (only-in "equation.rkt"
             make-equation eval-expr))
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo))
(require "wheel.rkt" predicates sugar)

(provide add-memorized-equations)

(define (remove-redundant-exprs exprs)
  (for/fold ([ht empty-hash] #:result (hash-values ht))
            ([expr exprs])
    (match-define `(,op . ,operands) expr)
    (define key
      (case op
        [(+ *) `(,op ,(apply set operands))]
        [(- /) expr]))
    (if (hash-has-key? ht key)
      ht
      (hash-set ht key expr))))

;TODO UT
(define (add-memorized-equations g exprs)
  (for/fold ([g g])
            ([expr (remove-redundant-exprs exprs)])
    (add-memorized-equation g (list (e:eval-expr expr) expr))))

(define (add-memorized-equation g eqn)
  (let*-values ([(g equation) (apply e:make-equation g eqn)]
                ;[(g) (add-equation-tags g equation)]
                )
    (sl:add-group-to-slipnet g equation)))

(define elementary-equation-exprs
  (append
    (cartesian-product
      '(+ - *)
      '(0 1 2 3 4 5 6 7 8 9 10)
      '(0 1 2 3 4 5 6 7 8 9 10))
    (cartesian-product
      '(+ - *)
      '(0 1 2 3 4 5 6 7 8 9 10)
      '(10))
    (cartesian-product
      '(+ -)
      (.. 0 100)
      '(0 1 2 3))
    (cartesian-product
      '(*)
      (.. 10 100 10)
      '(0 1 2 3 4 5 6 7 8 9 10))
    ))

;; ======================================================================
;;
;; Unit tests
;;

(module+ test
  (require expect/rackunit (only-in rackunit test-case))
  (require (only-in "fargish1.rkt"
             farg-model-spec nodeclass tagclass portclass)
           (only-in "make-slipnet.rkt"
             no-archetype is-node is-value is-class)
           (only-in "shorthand.rkt"
             make-graph))

  (define spec
    (farg-model-spec
      (nodeclass (equation nm)
        (is-a 'ctx)
        (name nm)
        (archetype is-node))
      (nodeclass (number n)
        (name n)
        (value n)
        (archetype n))
      (nodeclass operator
        (archetype is-class))
      (nodeclass +
        (is-a 'operator))
      (nodeclass -
        (is-a 'operator))
      (nodeclass *
        (is-a 'operator))
      (nodeclass /
        (is-a 'operator))))

  (define empty-g (make-graph spec '(ws) '(slipnet)))

  (test-case "add-memorized-equations"
    (define a-few-equations
      '((+ 2 3) (* 2 3)))
    (define g (m:copy-graph empty-g))
    (gdo add-memorized-equations a-few-equations)
    (check-equal? (list->set (sl:archetypes g))
                  (set 'archetype-2 'archetype-3 'archetype-5 'archetype-6
                       'archetype-+ 'archetype-* '2+3=5 '2*3=6))
    (check-equal? (m:port->neighboring-ports g '(2+3=5 activation))
                  (set '(archetype-+ activation)
                       '(archetype-2 activation)
                       '(archetype-3 activation)
                       '(archetype-5 activation)))
    (check-equal? (list->set (m:members-of g '2+3=5))
                  (set '2 '3 '5 '+))  ; equation members might be renamed
  ))
