; numbo1 -- A "hacked" Numbo that does most things in non-FARG-like ways
;           but not as bad as numbo0

#lang debug at-exp racket

(require errortrace)
(require "wheel.rkt" "xsusp3.rkt" "fargish.rkt"
         (prefix-in g: "graph1.rkt") (only-in "graph1.rkt" define/g gdo)
         (prefix-in g: "make-graph1.rkt")
         (only-in "make-graph1.rkt" make-graph))
(require racket/set racket/hash)
(require rackunit racket/pretty describe)

(provide (all-defined-out))

;; Global constants

(define max-timesteps 20)
(define slipnet-spreading-rate 0.01)
(define slipnet-decay 0.9)
(define slipnet-timesteps 3)
(define support-decay-rate 0.5)

;; ======================================================================
;;
;; The spec
;;

(define spec
  (farg-model-spec
    (nodeclass numbo-ws
      (is-a 'ctx))
    (nodeclass equation
      (is-a 'ctx))
    (nodeclass number)
    (nodeclass target
      (is-a 'number)
      (links-into 'ctx (by-ports 'target 'result) as-member))
    (nodeclass brick
      (is-a 'number)
      (links-into 'ctx (by-ports 'bricks 'source) as-member))
    (nodeclass operator)
    (nodeclass +
      (is-a 'operator))
    (nodeclass -
      (is-a 'operator))
    (nodeclass *
      (is-a 'operator))
    (nodeclass /
      (is-a 'operator))
    (tagclass (needs source)
      (is-a 'problem-tag)
      (applies-to ([node (of-class 'number) (by-ports 'tagged 'tags)])
        (condition (no-neighbor-at-port?/g 'source node))))
    ))

;; ======================================================================
;;
;; Making a workspace
;;

(define (make-numbo-ws g bricks target)
  (let*-values ([(g ws) (make-node g 'numbo-ws)]
                [(g) (add-nodes/in g ws 'brick bricks)]
                [(g) (add-node/in g ws 'target target)])
    (values g ws)))

(define (make-numbo-g . args)
  (first-value (apply make-numbo-ws (make-empty-graph spec) args)))

(module+ test
  (test-case "numbo-ws"
    (let*-values ([(g) (make-empty-graph spec)]
                  ;[(_) (pretty-print (get-spec g))]
                  [(g ws) (make-numbo-ws g '(4 5 6) 15)])
      ;(pr-graph g)
      ;TODO
      (void)
      )))

;; ======================================================================
;;
;; Searching the numbo-ws

(define (done? g)
  (define (has-source? node)
    (cond
      [(node-is-a? g 'number node)
       (define sources (port->neighbors g `(,node source)))
       (if (empty? sources)
         #f
         (for/and ([source sources])
           (has-source? source)))]
      [(node-is-a? g 'operator node)
       (define operands (port->neighbors g `(,node operands)))
       (if (empty? operands)
         #f
         (for/and ([operand operands])
           (has-source? operand)))]
      [(node-is-a? g 'numbo-ws node)
       #t]))
  ;TODO? throw error if no match?

  (has-source? (port->neighbor g '(numbo-ws target))))

(module+ test
  (let ([g (make-numbo-g '(4 5 6) 15)])
    (check-false (done? g))
    


    (let ([g (g:do-graph-edits g '((:begin (:node number 9) (:node operator + +)
                                   (:edge (4 result) (+ operands))
                                   (:edge (5 result) (+ operands))
                                   (:edge (+ result) (9 source)))))])
      (check-false (done? g))
      (let ([g (g:do-graph-edits g '((:let ([+ (:node operator + +)])
                                     (:edge (9 result) (+ operands))
                                     (:edge (6 result) (+ operands))
                                     (:edge (+ result) (15 source)))))])
        (check-true (done? g)) ))))

;; ======================================================================
;;
;; Equation graph
;;

(define (make-equation-graph n1 op n2 result)
  (let ([group-name (string->symbol (format "~a~a~a=~a" n1 op n2 result))]
        [n2-name (if (equal? n1 n2) (string->symbol (format "~aa" n2)) n2)]
        [result-name (if (or (equal? result n1) (equal? result n2))
                       (string->symbol (format "~ar" result))
                       result)])
    (make-graph `(:let ([:equation
                          (:group (:name ,group-name) (:class equation)
                            (:node (:attrs ((class . number)
                                            (value . ,n1)
                                            (name .  ,n1))))
                            (:node (:attrs ((class . number)
                                            (value . ,n2)
                                            (name .  ,n2-name))))
                            (:node (:attrs ((class . operator)
                                            (value . ,op)
                                            (name . ,op))))
                            (:node (:attrs ((class . number)
                                            (value . ,result)
                                            (name .  ,result-name))))
                            (:edge (,n1 result) (,op operands))
                            (:edge (,n2-name result) (,op operands))
                            (:edge (,op result) (,result-name source)))])
                    (add-tag (fills-port ,n1 result) :equation)
                    ,@(if (> result n1)
                       `((add-tag (fills-port-greater-result ,n1) :equation))
                       '())
                    (add-tag (fills-port ,n2 result) :equation)
                    ,@(if (> result n2)
                       `((add-tag (fills-port-greater-result ,n2) :equation))
                       '())
                    (add-tag (fills-port ,op result) :equation)
                    (add-tag (fills-port ,op operands) :equation)
                    (add-tag (fills-port ,result source) :equation)
                    ,@(if (equal? n1 n2)
                        `((add-tag (doubled-operands ,n1) :equation))
                        '())
                    ))))


; Adds an 'equation group containing the result, sourced to expr. Makes a
; new node for each number and operator. Adds edges between appropriate
; 'source and 'result ports.
(define (make-equation g result- expr)
  (let*-values ([(g equation) (make-node g 'equation)]
                [(g result) (make-node/in g equation 'number result-)]
                [(g operator) (make-expr/in g equation expr)]
                [(g) (add-edge g `((,operator result) (,result source)))])
    (values g equation)))
                
(define (make-expr/in g equation expr)
  (match-define `(,operator- . ,operands) expr)
  (let-values ([(g operator) (make-node/in g equation operator-)])
    (for/fold ([g g] #:result (values g operator))
               ([operand operands])
      (let*-values ([(g operand) (make-node/in g equation 'number operand)]
                    [(g) (add-edge g `((,operand result) (,operator operand)))])
        g))))

(module+ test
  (test-case "make-equation"
    (define g (make-empty-graph spec))
    (define equation (gdo make-equation 4 '(+ 2 2)))

    (define (value-of/g node) (value-of g node))
    (define (class-of/g node) (class-of g node))
    (define (source-of g node) (port->neighbor g `(,node source)))
    (define (result-of g node) (port->neighbor g `(,node result)))

    (check-equal? (sorted (map value-of/g (members-of g equation)))
                  (list (void) 2 2 4))
    (check-equal? (sorted (map class-of/g (members-of g equation)))
                  '(+ number number number))
))
