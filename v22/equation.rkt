; equation.rkt -- Functions to make subgraphs that represent equations

#lang debug at-exp racket

(require "wheel.rkt" "observe.rkt")
(require "model1.rkt"
         (prefix-in g: "graph1.rkt")
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo
           no-neighbor-at-port?/g has-neighbor-at-port?/g))

(require expect/rackunit (only-in rackunit test-case))
(require racket/pretty describe profile racket/enter racket/trace)

(provide (all-defined-out))

;; ======================================================================
;;
;; Equation graph
;;

; Adds an 'equation group containing the result, sourced to expr. Makes a
; new node for each number and operator. Adds edges between appropriate
; 'source and 'result ports.
(define (make-equation g result- expr)
  (let*-values ([(g equation) (make-node g
                                'equation (equation-name result- expr))]
                [(g result) (make-node/in g equation 'number result-)]
                [(g operator) (make-expr/in g equation expr)]
                [(g) (g:add-edge g `((,operator result) (,result source)))])
    (values g equation)))

(define (make-expr/in g equation expr)
  (match-define `(,operator- . ,operands) expr)
  (let-values ([(g operator) (make-node/in g equation operator-)])
    (for/fold ([g g] #:result (values g operator))
               ([operand operands])
      (let*-values ([(g operand) (make-node/in g equation 'number operand)]
                    [(g) (g:add-edge g
                           `((,operand result) (,operator operands)))])
        g))))

(define (equation-name result expr)
  (string->symbol
    (string-append
      (string-join (map ~a (cdr expr)) (~a (car expr)))
      @~a{=@result})))

(define base-namespace (make-base-namespace))
(define (eval-expr expr)
  (eval expr base-namespace))

(module+ test
  (require (prefix-in f: "fargish1.rkt")
           (only-in "fargish1.rkt"
             farg-model-spec nodeclass tagclass portclass))

  (test-case "make-equation"
    (define spec
      (farg-model-spec
        (nodeclass (number n)
          (name n)
          (value n))
        (nodeclass (equation nm)
          (is-a 'ctx)
          (name nm))
        (nodeclass +)))
    (define g (make-empty-graph spec))
    (define equation (gdo make-equation 4 '(+ 2 2)))

    (define (value-of/g node) (value-of g node))
    (define (class-of/g node) (class-of g node))
    (define (source-of g node) (g:port->neighbor g `(,node source)))
    (define (result-of g node) (g:port->neighbor g `(,node result)))

    (check-equal? (sorted (map value-of/g (members-of g equation)))
                  (list (void) 2 2 4))
    (check-equal? (sorted (map class-of/g (members-of g equation)))
                  '(+ number number number))))

