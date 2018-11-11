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

(define get-node-attr g:get-node-attr)

(define as-member f:as-member)
(define by-ports f:by-ports)

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

(define/g (add-node/in g . args)
  (first-value (apply make-node/in g args)))

;; ======================================================================
;;
;; Functions to query nodes
;;

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

(define (node-is-a? g node ancestor)
  (f:nodeclass-is-a? (get-spec g) (nodeclass*-of g node) ancestor))

;; ======================================================================
;;
;; Predicates
;;

(define/g (value-pred? g pred? . nodes)
  (values g
    (cond
      [(null? nodes) #t]
      [(null? (cdr nodes)) #t]
      [else (let* ([node1 (car nodes)] [v (value-of g node1)])
              (for/and ([node (cdr nodes)])
                (pred? v (value-of g node))))])))

;; ======================================================================
;;
;; Tagging
;;

; Returns a nodeclass* where all the class-attrs have the args filled in
; as provided in nodespec. nodespec can be a symbol, in which case there
; are no args, or a list where the nodeclass's name is the first element
; and the remaining elements are the args, e.g. '(fills-port result 5).
(define (realize-nodespec g nodespec)
  (match nodespec
    [`(,class-name . ,args)
     (f:realize-attrs (get-nodeclass* g class-name) args)]
    [(? symbol?)
     (f:realize-attrs (get-nodeclass* g nodespec) '())]
    [else (raise-arguments-error 'realize-nodespec
                                 @~a{Invalid nodespec: @|nodespec|.})]))

; taggee-info* ti must be fully realized (no args needed).
(define (taggee-info-match? g ti node)
  (let ([spec (get-spec g)]
        [nclass (class-of g node)])
    (for/or ([ofc (f:taggee-info*-of-classes ti)])
      (f:nodeclass-is-a? spec nclass ofc))))

; The condition function c must be fully realized (no args needed).
(define (condition-match? g c . nodes)
  (define cfunc-takes-g (apply c nodes))
  (call-with-values (λ () (cfunc-takes-g g))
    (case-lambda 
      [(result) result]
      [(g result . ignored) result])))

(define (tagclass-applies-to? g tagspec . nodes)
  (if (null? nodes)
    #f
    (let ([tagclass (realize-nodespec g tagspec)])
      (for/or ([applies-to (f:get-raw-nodeclass-attr tagclass 'applies-to)])
        (and (for/and ([ti (f:applies-to*-taggee-infos applies-to)]
                       [node nodes])
               (taggee-info-match? g ti node))
             (for/or ([c (f:applies-to*-conditions applies-to)])
               (apply condition-match? g c nodes)))))))

;; ======================================================================
;;
;; Printing
;;

(define (pr-nodeclass-of g nodeid)
  (pretty-print (f:realize-attrs (nodeclass*-of g nodeid)
                                 (args-of g nodeid))))

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

    ; display-names
    (check-equal? (get-node-attr g number22 'name) 22)
    (check-equal? (get-node-attr g brick7 'name) 7)

    ; values
    (check-equal? (get-node-attr g number22 'value) 22)
    (check-equal? (get-node-attr g brick7 'value) 7)

    ; linking into a ctx
    (check-true (g:port-neighbor? g `(,ws bricks) brick7))
    (check-equal? (list->set (g:port->neighboring-ports g `(,ws members)))
                  (set `(,number22 member-of) `(,brick7 member-of)))

    ; is-a ancestor relations
    (check-true (node-is-a? g brick7 'brick))
    (check-true (node-is-a? g brick7 'number))
    (check-true (node-is-a? g number22 'number))
    (check-false (node-is-a? g number22 'brick)))
  
  (test-case "tagging"
    (define spec
      (farg-model-spec
        (nodeclass (number n)
          (value n)
          (name n))
        (nodeclass (brick n)
          (is-a 'number)
          (links-into 'ctx (by-ports 'bricks 'source) as-member))
        (nodeclass (neither n)
          (value n))
        (tagclass (same nc)  ; same value, both is-a nc
          (applies-to ([node1 (of-class nc) (by-ports 'tagged 'tags)]
                       [node2 (of-class nc) (by-ports 'tagged 'tags)])
            (condition (value-pred?/g = node1 node2))))))

    (define g (make-empty-graph spec))

    (define ws (gdo make-node 'ws))
    (define number22 (gdo make-node/in 'ws 'number 22))
    (define brick7 (gdo make-node/in ws 'brick 7))
    (define brick22 (gdo make-node/in ws 'brick 22))
    (define neither22 (gdo make-node/in ws 'neither 22))

    ;(pr-graph g)

    (check-false (tagclass-applies-to? g '(same number) brick7 number22))
    (check-true (tagclass-applies-to? g '(same number) brick22 number22))
    (check-false (tagclass-applies-to? g '(same number) neither22 number22))))


(define spec
  (farg-model-spec
    (nodeclass (number n)
      (value n)
      (name n))
    (nodeclass (brick n)
      (is-a 'number)
      (links-into 'ctx (by-ports 'bricks 'source) as-member))
    (tagclass (same nc)  ; same value, both is-a nc
      (applies-to ([node1 (of-class nc) (by-ports 'tagged 'tags)]
                   [node2 (of-class nc) (by-ports 'tagged 'tags)])
        (condition (value-pred?/g = node1 node2))))
    ))

(define g (void))
(set! g (make-empty-graph spec))

(define ws (gdo make-node 'ws))
(define number22 (gdo make-node/in 'ws 'number 22))
(define brick7 (gdo make-node/in ws 'brick 7))
(define brick22 (gdo make-node/in ws 'brick 22))

;(tagclass-applies-to? g '(same number) brick7 number22)
;(tagclass-applies-to? g '(same number) brick22 number22)
