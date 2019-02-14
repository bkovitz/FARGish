; support.rkt -- The support network in a FARG model

#lang debug at-exp racket

(require "wheel.rkt")
(require (prefix-in su: "support-network.rkt")
         (prefix-in m: "model1.rkt")
         (prefix-in g: "graph1.rkt")
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo))

; from-elem : Node|Edge
; to-elem : Node|Edge
(struct Support (from-elem to-elem) #:prefab)


;; ======================================================================
;;
;; Establishing support
;;

;TODO Allow support weights to be specified; currently 1.0.
; (: add-mutual-support : Graph Node|Edge Node|Edge -> Graph)
(define (add-mutual-support g elem1 elem2)
  (m:graph-update-var g 'supporting
    (λ (ht)  ; (Hashof Node/Edge (Hashof Node/Edge Flonum))
      (let* ([ht (hash-set-set elem1 elem2 1.0)]
             [ht (hash-set-set elem2 elem1 1.0)])
        ht))
    (hash)))

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

; This is for REPL experimentation and debugging, not model code. Normally
; support for all nodes should be set at once by calling set-support-ht.
(define (set-support-for g elem s)
  (let* ([ht (get-support-ht g)]
         [ht (hash-set ht elem s)])
    (set-support-ht g ht)))

; HACK: The 3.0 should probably be a number proportional to something
; about the state of the model.
(define normalize-support (curry su:normalize-by-reverse-sigmoid 3.0))

; Helper for support->t+1
(define (^support->t+1 ht/all-given old-ht)
  (let* ([node->targets (λ (node)
                          (hash-ref/sk ht/all-given node
                            hash-keys ;sk
                            '()))] ;fk
         [from-to->support-given (λ (from-node to-node)
                                   (hash-ref/sk ht/all-given from-node
                                     (λ (ht/given) ;sk
                                       (hash-ref ht/given to-node 0.0))
                                     (const 0.0)))] ;fk
         [old-ht (for/fold ([old-ht old-ht])
                           ([node (hash-keys ht/all-given)])
                   (if (hash-has-key? old-ht node)
                     old-ht
                     (hash-set old-ht node 0.0)))])
    (su:support-t+1 su:default-support-config
                    normalize-support
                    node->targets
                    from-to->support-given
                    old-ht)))

; Updates support for all nodes and edges.
(define (support->t+1 g)
  (let* ([old-ht (get-support-ht g)]
         [ht/all-given (get-supporting-ht g)]
         [new-ht (^support->t+1 ht/all-given old-ht)])
    (set-support-ht g new-ht)))

;TODO Unit tests
