; model1.rkt -- Generic run-time code for FARG model
;
; Goes with numbo1.rkt. Probably will be obsolete soon after numbo1.rkt is done.

#lang debug at-exp racket

(require errortrace)
(require "wheel.rkt"
         "xsusp3.rkt"
         (prefix-in f: "fargish1.rkt")
         (only-in "fargish1.rkt"
           farg-model-spec nodeclass tagclass)
         (prefix-in g: "graph1.rkt")
         ;(prefix-in g: "make-graph1.rkt")
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo
           no-neighbor-at-port?/g has-neighor-at-port?/g))
(require racket/set racket/hash)
(require rackunit racket/pretty describe)

(provide (all-defined-out))

;; Global constants

;TODO Global constants should be stored as variables in the graph.
(define max-timesteps 20)
(define slipnet-spreading-rate 0.01)
(define slipnet-decay 0.9)
(define slipnet-timesteps 3)
(define support-decay-rate 0.5)

(define (make-empty-graph spec)
  (struct-copy g:graph g:empty-graph [spec spec]))


;; ======================================================================
;;
;; Access to a few things without a prefix
;;

(define as-member f:as-member)
(define by-ports f:by-ports)

;; ======================================================================
;;
;; Printing
;;

(define (pr-nodeclass-of g nodeid)
  (define args (args-of g nodeid))
  (define nc (nodeclass*-of g nodeid))
  (define realized-attrs (for/hash ([(k v) (f:nodeclass*-class-attrs nc)])
                           (values k (f:apply-class-attr v args))))
  (pretty-print (struct-copy f:nodeclass* nc
                  [class-attrs realized-attrs])))

;; ======================================================================
;;
;; Functions to access elements of a FARG model specification
;;
;; Where appropriate, these functions' first argument can be either a graph
;; (that contains a spec) or a spec.
;;

(define (get-spec g-or-spec)
  (cond
    [(f:farg-model-spec*? g-or-spec) g-or-spec]
    [(g:graph? g-or-spec) (g:graph-spec g-or-spec)]
    [else (raise-arguments-error 'get-spec
                                 @~a{Can't get spec from @|g-or-spec|.})]))

(define (get-nodeclasses x)
  (cond
    [(f:farg-model-spec*? x) (f:farg-model-spec*-nodeclasses x)]
    [(g:graph? x) (get-nodeclasses (g:graph-spec x))]
    [else (raise-arguments-error 'get-nodeclasses
                                 @~a{Can't get nodeclasses from @|x|.})]))

(define (get-nodeclass* g-or-spec nc-or-classname)
  (cond
    [(f:nodeclass*? nc-or-classname) nc-or-classname]
    [else (f:get-nodeclass* (get-spec g-or-spec) nc-or-classname)]))

(define (nodeclass*-of g node)
  (hash-ref (get-nodeclasses g) (class-of g node)))
  ;TODO Appropriate response if unknown class

(define (get-links-into g node ctx)
  (define nc (nodeclass*-of g node))
  (define ctx-class (class-of g ctx))
  (define args (get-node-attr g node 'args))
  (define spec (get-spec g))
  (let ([lis (filter (λ (li)
                       (f:nodeclass-is-a? spec
                         ctx-class
                         (f:links-into*-ctx-class li)))
                     (f:get-nodeclass-attr nc 'links-into args))])
    (if (null? lis) (list (f:links-into* ctx (list as-member))) lis)))

;; ======================================================================
;;
;; Functions to make nodes
;;

(define/g (make-node g classname . args)
  (let* ([nodeclass (get-nodeclass* g classname)]
         [attrs (f:args->node-attrs nodeclass args)])
    (g:make-node g attrs)))

(define/g (add-node g . args)
  (first-value (apply make-node g args)))

(define/g (make-node/in g ctx classname . args)
  ;TODO Raise error if ctx does not exist
  (let-values ([(g node) (apply make-node g classname args)])
    (for*/fold ([g g] #:result (values g node))
               ([links-into (get-links-into g node ctx)]
                [by-ports (f:links-into*-by-portss links-into)])
      (match-define (f:by-ports* from-port-label to-port-label) by-ports)
      (g:add-edge g `((,ctx ,from-port-label) (,node ,to-port-label))))))

;; ======================================================================
;;
;; Functions to query nodes
;;

(define get-node-attr g:get-node-attr)

(define (args-of g node)
  (get-node-attr g node 'args))

(define (name-of g node)
  (get-node-attr g node 'name))

(define (class-of g node)
  (get-node-attr g node 'class))

(define (value-of g node)
  (get-node-attr g node 'value))

(define (value-of-equal? g v node)
  (define node-value (value-of g node))
  (and (not (void? node-value)) (equal? v node-value)))

(define (tag? g node)
  (g:node-attr? g node 'tag?))

;; ======================================================================
;;
;; Unit tests
;;

(module+ test
  (test-case "spec basics"
    (define spec
      (farg-model-spec
        (nodeclass (number n)
          (value n)
          (name n))
        (nodeclass (brick n)
          (is-a 'number)
          (links-into 'ctx (by-ports 'bricks 'source) as-member))))

    (define g (make-empty-graph spec))

    (define ws (gdo make-node 'ws))
    (define number22 (gdo make-node/in 'ws 'number 22))
    (define brick7 (gdo make-node/in ws 'brick 7))

    (check-equal? (list->set (g:all-nodes g))
                  (set ws number22 brick7))
    (check-true (g:port-neighbor? g `(,ws bricks) brick7))
    (check-equal? (list->set (g:port->neighboring-ports g `(,ws members)))
                  (set `(,number22 member-of) `(,brick7 member-of)))
    ))


(define spec
  (farg-model-spec
    (nodeclass (number n)
      (value n)
      (name n))
    (nodeclass (brick n)
      (is-a 'number)
      (links-into 'ctx (by-ports 'bricks 'source) as-member))))

(define g (void))
(set! g (make-empty-graph spec))
