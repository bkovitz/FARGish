; numbo1 -- A "hacked" Numbo that does most things in non-FARG-like ways
;           but not as bad as numbo0

#lang debug at-exp racket

(require errortrace)
(require "wheel.rkt"
         "xsusp3.rkt"
         "model1.rkt"
         "slipnet1.rkt"
         "completion1.rkt"
         (prefix-in f: "fargish1.rkt")
         (only-in "fargish1.rkt"
           farg-model-spec nodeclass tagclass)
         (prefix-in g: "graph1.rkt")
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo
           no-neighbor-at-port?/g has-neighbor-at-port?/g))
(require racket/set racket/hash)
(require rackunit racket/pretty describe)

(provide (all-defined-out))

;; Global constants

(define max-timesteps 20)
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
      (is-a 'ctx)
      (archetype is-node))
    (nodeclass (number n)
      (name n)
      (value n)
      (archetype n))
    (nodeclass (target n)
      (is-a 'number)
      (links-into 'ctx (by-ports 'target 'result) as-member))
    (nodeclass (brick n)
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
    (tagclass problem-tag)
    (tagclass (needs need-type)
      (is-a 'problem-tag)
      (name (string->symbol @~a{needs-@need-type}))
      (archetype `(needs ,need-type))
      (value `(needs ,need-type))
      (applies-to ([node (of-class 'number) (by-ports 'tagged 'tags)])
        (condition (no-neighbor-at-port?/g need-type node))))
    (tagclass (fills-port result n)
      (is-a 'solution-tag)
      (archetype `(fills-port ,result ,n))
      (applies-to ([node (of-class 'equation) (by-ports 'tagged 'tags)])
        (condition (const #t)))
;      (applies-to ([node (by-ports 'tagged 'tags)])
;        (condition (has-neighbor-at-port?/g result node)))
      )
    ))

;; ======================================================================
;;
;; Making a workspace
;;

(define (make-numbo-ws g bricks target)
  (let*-values ([(g ws) (make-node g 'ws)]
                [(g) (add-nodes/in g ws 'brick bricks)]
                [(g) (add-node/in g ws 'target target)])
    (values g ws)))

(define (make-numbo-g . args)
  (let* ([g (first-value (apply make-numbo-ws (make-empty-graph spec) args))]
         [g (add-node g 'slipnet)])
    g))

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
;;

(define (done? g)
  (define (has-source? node)
    (cond
      [(node-is-a? g 'number node)
       (define sources (g:port->neighbors g `(,node source)))
       (if (empty? sources)
         #f
         (for/and ([source sources])
           (has-source? source)))]
      [(node-is-a? g 'operator node)
       (define operands (g:port->neighbors g `(,node operands)))
       (if (empty? operands)
         #f
         (for/and ([operand operands])
           (has-source? operand)))]
      [(node-is-a? g 'numbo-ws node)
       #t]))
  ;TODO? throw error if no match?

  (has-source? (g:port->neighbor g '(numbo-ws target))))

;(module+ test
;  (let ([g (make-numbo-g '(4 5 6) 15)])
;    (check-false (done? g))
;    
;
;
;    (let ([g (g:do-graph-edits g '((:begin (:node number 9) (:node operator + +)
;                                   (:edge (4 result) (+ operands))
;                                   (:edge (5 result) (+ operands))
;                                   (:edge (+ result) (9 source)))))])
;      (check-false (done? g))
;      (let ([g (g:do-graph-edits g '((:let ([+ (:node operator + +)])
;                                     (:edge (9 result) (+ operands))
;                                     (:edge (6 result) (+ operands))
;                                     (:edge (+ result) (15 source)))))])
;        (check-true (done? g)) ))))

;; ======================================================================
;;
;; Equation graph
;;

; Adds an 'equation group containing the result, sourced to expr. Makes a
; new node for each number and operator. Adds edges between appropriate
; 'source and 'result ports.
(define (make-equation g result- expr)
  (let*-values ([(g equation) (make-node g 'equation)]
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

;TODO UT
(define (add-memorized-equations g)
  (for/fold ([g g])
            ([eqn (list '(9 (+ 4 5))
                        '(15 (+ 9 6)))])
    (let*-values ([(g equation) (apply make-equation g eqn)])
      (add-equation-tags g equation)
      (add-group-to-slipnet g equation))))

(module+ test
  (test-case "make-equation"
    (define g (make-empty-graph spec))
    (define equation (gdo make-equation 4 '(+ 2 2)))

    (define (value-of/g node) (value-of g node))
    (define (class-of/g node) (class-of g node))
    (define (source-of g node) (g:port->neighbor g `(,node source)))
    (define (result-of g node) (g:port->neighbor g `(,node result)))

    (check-equal? (sorted (map value-of/g (members-of g equation)))
                  (list (void) 2 2 4))
    (check-equal? (sorted (map class-of/g (members-of g equation)))
                  '(+ number number number))
))

(define (add-fills-port-tags g equation)
  (for*/fold ([g g])
             ([node (members-of g equation)]
              [port-label `(source result)])
    (let ([value (value-of g node)])
      (cond
        [(void? value) g]
        [else
          (if (g:port-has-neighbor? g `(,node ,port-label))
            (add-tag g `(fills-port ,value ,port-label) equation)
            g)]))))
    
(define (add-equation-tags g equation)
  (let* ([g (add-fills-port-tags g equation)])
    g))

(module+ test
  (test-case "add-equation-tags"
    (define g (make-empty-graph spec))
    (define equation (gdo make-equation 4 '(+ 2 2)))
    
    (gdo add-equation-tags equation)))

;; ======================================================================
;;
;; Searching the slipnet
;;

(define (search-slipnet g initial-activations)
  (define activations (run-slipnet g initial-activations))
  ;#R (sorted-by-cdr activations)
  (most-active-equation g activations))

(define (most-active-equation g activations)
  (define eqn-activations (for/list ([e-a (hash->list activations)]
                                     #:when (node-is-a? g (car e-a) 'equation))
                            e-a))
  (when (null? eqn-activations)
    (raise 'nothing-to-do))
  (car (argmax cdr eqn-activations)))

;TODO Move to model1.rkt
(define (neighborhood-around g node [max-steps 2])
  (let loop ([num-steps 1]
             [result (set `(,node 0))]
             [neighbors (g:node->neighbors g node)]
             [already-visited (set node)])
    (cond
      [(> num-steps max-steps) result]
      [(set-empty? neighbors) result]
      [else (let* ([new-neighbors (set-subtract neighbors already-visited)]
                   [result (set-union result (for/set ([neighbor new-neighbors])
                                               `(,neighbor ,num-steps)))])
              (loop (add1 num-steps)
                    result
                    (apply set-union (for/list ([neighbor new-neighbors])
                                       (g:node->neighbors g neighbor)))
                    (set-union already-visited new-neighbors)))])))

;NEXT Convert to archetypes
(define (make-initial-activations g focal-node)
  (for/fold ([ht (hash)]) ; (archetype . activation)
            ([pair (neighborhood-around g focal-node)])
    (match-define `(,node ,num-steps) pair)
    (define activation (* (salience-of g node) (expt 0.8 num-steps)))
    (if (zero? activation)
      ht
      (hash-set ht node activation))))

;; ======================================================================
;;
;; Diagnosing problems, mapping to solutions
;;

(define problem-tags '((needs source) (needs result)))

;TODO Move to model1.rkt
(define (applicable-tags g node tagspecs)
  (for/list ([tagspec tagspecs]
             #:when (tagclass-applies-to? g tagspec node))
    tagspec))

; Returns two values: g, node-with-problem(s).
; "Side-effect": adds appropriate problem tag(s) to the node with problems.
(define (look-for-problems g ctx)
  (let-values ([(node problem-tags) (find-node-with-problems g ctx)])
    (if (void? node)
      (values g (void))
      (values (add-tags g problem-tags node) node))))

; Returns two values: node, problem-tags. If no problem found, then (void)
; (void).
(define (find-node-with-problems g ctx)
  (let loop ([nodes (seq-weighted-by-salience g (members-of g ctx))])
    (cond
      [(null? nodes) (values (void) (void))]
      [else (let* ([node (car nodes)]
                   [tagspecs (applicable-tags g node problem-tags)])
              (cond
                [(null? tagspecs) (loop (cdr nodes))]
                [else (values node tagspecs)]))])))

(define (remove-obsolete-tags g ctx)
  (for*/fold ([g g])
             ([node (members-of g ctx)]
              [tag (tags-of g node)])
    (if (tag-still-applies? g tag)
      g
      (remove-tag g tag))))

(define (remove-tag g tag)
  (let ([nodes (taggees-of g tag)])
    (for/fold ([g (g:remove-node g tag)])
              ([node nodes])
      (reduce-salience-of g node))))

;; ======================================================================
;;
;; Running
;;

(define (do-timestep g)
  (with-handlers ([(eq?? 'nothing-to-do) (λ (_) (log "Nothing to do.") g)])
    (let*-values ([(g) (decay-saliences-in g 'ws)]
                  ;[focal-node (choose-focal-node g 'ws)]
                  [(g focal-node) (look-for-problems g 'ws)]
                  ;[(_) (pr-group g 'ws)]
                  [(activations) (maybe-suspend 'slipnet-activations
                                              (make-initial-activations g focal-node))]
                  [(equation) (search-slipnet g activations)]
                  [(_) (log "trying" equation)]
                  [(g) (clear-touched-nodes g)]
                  [(g) (complete-partial-instance-in g equation 'ws)]
                  [(g) (remove-obsolete-tags g 'ws)]
                  [(g) (boost-salience-of-touched-nodes g)])
      g)))

(define (run^ g)
  (maybe-suspend 'g g)
  (with-handlers ([(λ (e) (match e
                            [`(done ,_) #t]
                            [else #f]))
                   (λ (e) (cadr e))])
    (for/fold ([g g])
              ([t max-timesteps])
      (if (done? g)
        (begin
          ;(log (result-expr g))
          (raise `(done ,g)))
        (maybe-suspend 'g (do-timestep g))))))

(define (run bricks target)
  (let*-values ([(g) (make-numbo-g bricks target)]
                [(g) (add-memorized-equations g)])
    (run^ g)))

(define (choose-focal-node g ctx)
  (safe-car
    (seq-weighted-by-salience g (members-of g 'ws))))

(define g (make-numbo-g '(4 5 6) 15))
(gdo add-memorized-equations)

(pr-graph g)
