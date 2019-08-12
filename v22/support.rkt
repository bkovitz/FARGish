; support.rkt -- The support network in a FARG model

#lang debug at-exp racket

(require "wheel.rkt")
(require (prefix-in su: "support-network.rkt")
         (prefix-in m: "model1.rkt")
         (prefix-in g: "graph1.rkt")
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo))

(provide (all-defined-out))

; from-elem : Node|Edge
; to-elem : Node|Edge
(struct Support (from-elem to-elem) #:prefab)


;; ======================================================================
;;
;; Establishing support
;;

; (: normalize-elem : Node/Edge -> (U Node EdgeSet)) ; TODO case->
(define (normalize-elem elem)
  (cond
    [(or (symbol? elem) (integer? elem))
     elem]
    [else (m:edge->set elem)]))

;TODO Allow support weights to be specified; currently 1.0.
; (: add-mutual-support : Graph Node|Edge Node|Edge -> Graph)
(define (add-mutual-support g elem1 elem2)
  (m:graph-update-var g 'ht/supporting
    (let ([elem1 (normalize-elem elem1)]
          [elem2 (normalize-elem elem2)])
      (λ (ht)  ; (Hashof Node/EdgeSet (Hashof Node/EdgeSet Flonum))
        (let* ([ht (hash-set-set ht elem1 elem2 1.0)]
               [ht (hash-set-set ht elem2 elem1 1.0)])
          ht)))
    (hash)))

;(define (add-self-support g elem [amount 0.5]) ;GLOBAL constant
;  (m:graph-update-var g 'ht/elem->self-support
;    (curry + amount)
;    0.0))

;; ======================================================================
;;
;; Updating the support network (flow of support)
;;

; Sets the hash table for all total support for all nodes.
(define (set-support-ht g ht/elem->support)
  (m:graph-set-var g 'ht/elem->support ht/elem->support))

(define (get-support-ht g)
  (m:graph-get-var g 'ht/elem->support empty-hash))

(define (support-for g elem)
  (hash-ref (get-support-ht g) elem 0.0))

;(: get-supporting-ht : Graph -> (Hashof Node/Edge (Hashof Node/Edge Flonum)))
(define (get-supporting-ht g)
  (m:graph-get-var g 'ht/supporting empty-hash))

(define (set-support-for g elem s)
  (let* ([ht (get-support-ht g)]
         [ht (hash-set ht elem s)])
    (set-support-ht g ht)))

(define (make-permanent g . nodes)
  (for/fold ([g g])
            ([node nodes])
    (let ([g (m:set-node-attr g node 'permanent-support 1.0)])
      (if (< (support-for g node) 1.0)
        (set-support-for g node 1.0)
        g))))

; HACK: max-support should probably be a number proportional to something
; about the state of the model.
(define max-support 40.0) ;GLOBAL
(define normalize-support (curry su:normalize-by-reverse-sigmoid max-support))

; Helper for support->t+1
(define (^support->t+1 ht/all-given elem->perm-support old-ht)
  (let* ([node->targets (λ (node)
                          (hash-ref/sk ht/all-given node
                            hash-keys ;sk
                            '()))] ;fk
         [from-to->support-given (λ (from-node to-node)
                                   (hash-ref/sk ht/all-given from-node
                                     (λ (ht/given) ;sk
                                       (hash-ref ht/given to-node 0.0))
                                     (const 0.0)))] ;fk
         )
    (su:support-t+1 su:default-support-config
                    normalize-support
                    node->targets
                    from-to->support-given
                    elem->perm-support
                    old-ht)))

; Updates support for all nodes and edges.
(define (support->t+1 g)
  (let* ([old-ht (get-support-ht g)]
         [ht/all-given (get-supporting-ht g)]
         [old-ht (for/fold ([old-ht old-ht])
                           ([node (hash-keys ht/all-given)])
                   (if (hash-has-key? old-ht node)
                     old-ht
                     (hash-set old-ht node 0.0)))]
         [old-ht (for/fold ([ht old-ht])
                           ([(elem supp) old-ht])
                   (let ([sal (m:salience-of g elem)])
                     (if (< supp sal)
                       (hash-set ht elem sal)
                       ht)))]
         [elem->perm-support
           (λ (elem)
             (cond
               [(m:node? elem) (m:get-node-attr g elem 'permanent-support 0.0)]
               [else 0.0]))]   ;TODO For edge, probably should take min of
                               ;permanent support of incident nodes.
         [new-ht (^support->t+1 ht/all-given elem->perm-support old-ht)])
    (set-support-ht g new-ht)))

(define (^one-line elem supp)
  (let ([lhs (cond
               [(m:node? elem) elem]
               [else (match (m:edge->list elem)
                       ;TODO Alphabetize by port label
                       [`(,port1 ,port2) @~a{@port1 -- @port2}])])])
    (string-append (~a lhs #:align 'right
                           #:min-width 35)
                   "  "
                   (~r supp #:precision (list '= 3)))))

(define (pr-support g)
  (for ([line (sorted (for/list ([(elem supp) (get-support-ht g)])
                        (^one-line elem supp)))])
    (displayln line)))

;TODO Unit tests
