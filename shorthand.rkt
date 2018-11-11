; shorthand.rkt -- Simple shorthand for specifying edits to a graph

#lang debug at-exp racket

(require errortrace)

(require "wheel.rkt"
         "xsusp3.rkt"
         (prefix-in f: "fargish1.rkt")
         (only-in "fargish1.rkt"
           farg-model-spec nodeclass tagclass by-ports as-member)
         (prefix-in g: "graph1.rkt")
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo)
         (prefix-in m: "model1.rkt"))

(require rackunit racket/pretty describe)

(provide do-graph-edits do-graph-edit make-graph)

;; ======================================================================
;;
;; make-graph, do-graph-edits, and do-graph-edit
;;

(define (make-graph spec . edits)
  (do-graph-edits (m:make-empty-graph spec) edits))

(define (do-graph-edits g edits)
  (apply do-graph-edit g edits))

(define (do-graph-edit g . edits)
  (for/fold ([g g])
            ([edit edits])
    (match edit
      [`(:in ,group . ,edits)
        (let*-values ([(g group) (get/make g group)]
                      [(g) (push-groupid g group)]
                      [(g) (do-graph-edits g edits)]
                      [(g) (pop-groupid g)]
                      [(g) (set-lastid g group)])
          g)]
      [`(:edge ,nodespec1 ,port-label1 ,nodespec2 ,port-label2)
        (let*-values ([(g node1) (get/make g nodespec1)]
                      [(g node2) (get/make g nodespec2)]
                      [(g) (g:add-edge g
                             `((,node1 ,port-label1) (,node2 ,port-label2)))])
          g)]
      [`(:let ([,id ,nodespec] ...) . ,edits)
        (for/fold ([g (push-aliases g)] #:result (do-graph-edits g edits))
                  ([id* id] [nodespec* nodespec])
          (let*-values ([(g nodeid) (get/make g nodespec*)])
            (set-alias g id*)))]
      [nodespec
        (first-value (get/make g nodespec))]
      )))

(define (make-node/sh g . args)
  (let*-values ([(g nodeid) (let ([groupid (current-groupid g)])
                              (if (void? groupid)
                                (apply m:make-node g args)
                                (apply m:make-node/in g groupid args)))]
                [(g) (set-lastid g nodeid)])
    (values g nodeid)))

; Returns two values: g nodeid
(define (get/make g nodespec)
  (match nodespec
    [`(,nodeclass . ,args)
      (apply make-node/sh g nodeclass args)]
    [nodeid
      ;TODO Throw exc if nodeid does not exist?
      (values g (look-up-node g nodeid))]))

;; ======================================================================
;;
;; Ancillary functions to track variables: current-groupid, lastid, aliases
;;

(define (current-groupid g)
  (g:graph-get-var g 'groupid (void)))
  ;(dict-ref (graph-stacks g) 'groupid #f))

(define (push-groupid g groupid)
  (g:graph-push-and-set-var g 'groupid groupid))

(define (pop-groupid g)
  (g:graph-pop-var g 'groupid))

(define (set-lastid g id)
  (g:graph-set-var g 'lastid id))

(define (get-lastid g)
  (g:graph-get-var g 'lastid))

(define (set-alias g alias)
  (g:graph-update-var g 'hm-alias->id
    (λ (hm) (hash-set hm alias (get-lastid g)))
    #hash()))

(define (push-aliases g)
  (g:graph-push-var g 'hm-alias->id))

(define (pop-aliases g)
  (g:graph-pop-var g 'hm-alias->id))

(define (look-up-node g name)
  (let ([hm-alias->id (g:graph-get-var g 'hm-alias->id #hash())])
    (hash-ref hm-alias->id name (thunk (if (g:has-node? g name) name (void))))))

;; ======================================================================
;;
;; Unit tests
;;

(module+ test
  (define spec
    (farg-model-spec
      (nodeclass (number n)
        (value n)
        (name n))
      (nodeclass (brick n)
        (is-a 'number)
        (links-into 'ctx (by-ports 'bricks 'source) as-member))
      (nodeclass +)
      (nodeclass equation
        (is-a 'ctx))))

  (test-case "shorthand basics"
    (define g (m:make-empty-graph spec))

    (define slipnet (gdo m:make-node 'slipnet))

    (gdo do-graph-edit '(:in slipnet
                          (:in (equation)
                            (:let ([15 (number 15)]
                                   [+ (+)]
                                   [9 (number 9)]
                                   [6 (number 6)])
                              (:edge 15 source + result)
                              (:edge 9 result + operands)
                              (:edge 6 result + operands)))))

    (check-equal? (list->set (g:all-nodes g))
                  (list->set '(slipnet equation 15 + 9 6)))
    (check-equal? (list->set (g:members-of g 'slipnet))
                  (list->set '(equation)))
    (check-equal? (list->set (g:members-of g 'equation))
                  (list->set '(15 + 9 6)))
    (check-true (g:has-edge? g '((+ result) (15 source))))
    (check-true (g:has-edge? g '((9 result) (+ operands))))
    (check-true (g:has-edge? g '((6 result) (+ operands)))))
  
  (test-case "make-graph, just make some nodes"
    (define g (make-graph spec '(ws) '(slipnet) '(:in ws
                                                   (brick 7))))

    (check-equal? (list->set (g:all-nodes g))
                  (list->set '(ws slipnet 7)))
    (check-true (g:has-edge? g '((ws bricks) (7 source))))
    (check-equal? (list->set (g:members-of g 'ws))
                  (list->set '(7)))))
