; crawl.rkt -- Crawl a slipnet

#lang debug at-exp racket

(require "wheel.rkt"
         "observe.rkt"
         "model1.rkt"
         "slipnet1.rkt"
         (prefix-in f: "fargish1.rkt")
         (prefix-in g: "graph1.rkt")
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo
           no-neighbor-at-port?/g has-neighbor-at-port?/g))
(require data/gvector plot pict pict/convert)
(require racket/hash)
(require racket/flonum racket/unsafe/ops)
(require racket/generic)
(require rackunit)
(require racket/pretty describe profile racket/enter racket/trace)

(provide acceptability make-inexact)

'(crawler
   (in-ws ([t (node-available-as target)]
           [bs (nodes-available-as brick)])
     (seek-instance-of equation
       (prefer (tagged-with? (has-result (inexact-number t))))
       (require (tagged-with? (has-operand (exact-number bs)))
                (count-at-least 2)))))

; NEXT Implement this simple crawler, without compiling it from a macro
'(crawler
   (in-ws ([t (node-available-as target)])
     (require (tagged-with? (has-result (exact-number t))))))

; IDEA A "continuation" crawler or search-item, to represent the progression
; from searching for nodes to bind for in-ws and filling in the references
; inside the require-tag*. Then, initially, the require-tag* could have a
; reference inside it, which the continuation simply replaces. There could be
; a generic function to extract the references from a search-item. When this
; function returns an empty list, then it's time to start searching for
; that search-item. This might simplify a lot of code (including the macro).

; IDEA Specify the types of edges that a crawler should follow. Instead of
; 'activation ports on slipnodes, label them by the kind of connection:
; archetype, instance, aggregate, part, property. Then crawlers can explore
; "agnostically" wrt to slipnet or workspace.

(define-generics search-item
  [acceptability g search-item node])

(struct inexact-number* (n) #:transparent)
; n : a primitive number

(define make-inexact inexact-number*)


(struct in-ws* (prebindings k-search-item) #:transparent)
; prebindings : (Listof prebinding*)
; k-search-item : search-item* to fill with prebindings and run when the
;   prebindings are completed.

(struct prebinding* (name search-item) #:transparent)
; name : Symbol
; search-item : search-item* that we will search for in ws to prebind to name.

(define (prebind


(struct require-tag* (class n) #:transparent
; class : a symbol, the name of a nodeclass
; n : a primitive number or an inexact-number*
  #:methods gen:search-item
  [(define (acceptability g search-item node)
     (define n (require-tag*-n search-item))
     (define class (require-tag*-class search-item))
     (apply safe-max 0.0 (for/list ([tag (tags-of-class g class node)])
                           (number-match g n tag))))])

;(let* ([target (node-available-as g 'target)]
;       [t (->number g target)])
;  (require-tag* 'has-operand t))
;
;
;(struct live-crawler* (search-item activations num-steps-taken) #:transparent
;; search-item : a search-item* (possibly compound)
;; activations : (Hashof node . activation)
;; num-steps-taken : integer
;
;(define (start-crawler g crawler-definition)
;  (live-crawler* (make-search-item g crawler-definition)
;                 initial-activations
;                 0))
;
;(define (crawl-step g live-crawler)
;  (let* ([search-item (live-crawler*-search-item live-crawler)]
;         [activations (live-crawler*-activations live-crawler)]
;         [num-steps-taken (live-crawler*-num-steps-taken live-crawler)])
;    (struct-copy live-crawler* live-crawler
;                 [activations (spread-activation
;                                activations)
;                                (λ (archetype)
;                                  (archetype-edges g archetype))
;                                (λ (node1 node2)
;                                  (edge-weight g search-item node1 node2))]
;                 [num-steps-taken (add1 num-steps-taken)])))
;
;(define (crawler-found g live-crawler)
;  ;Look through activations, find best nodes that exceed some activation
;  ;and acceptability threshold.
;  'STUB)

(struct seek-instance-of* (archetype) #:transparent
; archetype : a symbol, the name of a nodeclass
;   TODO change to an archetype node, i.e. a node linked to via instance-of
  #:methods gen:search-item
  [(define (acceptability g search-item node)
     (define archetype (seek-instance-of*-archetype search-item))
     (if (node-is-a? g node archetype) 1.0 0.0))])

; Returns 0.0..1.0 reflecting how well n matches m.
;
; The function is symmetric: it's the max of the match functions for n and m.
;
; n and m can be any type that converts to a number per ->number.
(define (number-match g n m)
  (define n->match (->numeric-matchf g n))
  (define m->match (->numeric-matchf g m))
  (define n-exact (->exact-number g n))
  (define m-exact (->exact-number g m))
  (safe-max 0.0 (n->match m-exact) (m->match n-exact)))

; Returns a function that returns 0.0..1.0 or void, reflecting how well
; a given raw number matches x.
;
; x can be any type that converts to a number per ->number.
(define (->numeric-matchf g x)
  (let ([x (->number g x)])
    (cond
      [(void? x)
       (λ (y) (if (void? y) 1.0 0.0))]
      [(number? x)
       (λ (y) (if (= x y) 1.0 0.0))]
      [(inexact-number*? x)
       (let ([x (inexact-number*-n x)])
         (λ (y) (exp (- (absdiff x y)))))])))
                ; TODO Allow the model to adjust a parameter to this function

; Converts x to a primitive number, inexact-number*, or void.
;
; x can be a number, inexact-number*, list, symbol, or void. If a list,
; we take the first number. If a symbol, we treat it as a nodeid and look up
; its 'value or 'args (if the node is a tag).
(define (->number g x)
  (cond
    [(number? x) x]
    [(inexact-number*? x) x]
    [(void? x) x]
    [(symbol? x)
     (->number g (if (tag? g x)
                   (args-of g x)
                   (value-of g x)))]
    [(pair? x)
     (first-fargnumber x)]))

; Converts x to a primitive number or void. An inexact-number* is converted
; to the exact number at its center.
;
; x can be any type that converts to a number per ->number.
(define (->exact-number g x)
  (let ([x (->number g x)])
    (cond
      [(number? x) x]
      [(inexact-number*? x) (inexact-number*-n x)]
      [(void? x) x])))

; Returns first primitive number or inexact-number* in lst, or void if none
; found.
(define (first-fargnumber lst)
  (cond
    [(null? lst) (void)]
    #:define a (car lst)
    [(number? a) a]
    [(inexact-number*? a) a]
    [else (first-fargnumber (cdr lst))]))

(module+ test
  (require "fargish1.rkt")
  (define spec
    (farg-model-spec
      (nodeclass (brick n)
        (name n)
        (value n))
      (nodeclass (equation nm)
        (name nm))
      (tagclass (has-operand n)
        (applies-to ([eqn (of-class 'equation) (by-ports 'tagged 'tags)])
          (condition (const #t))))))

  (define empty-g (make-empty-graph spec))

  (test-case "test ->number"
    (check-equal? (->number empty-g 3) 3)
    (let*-values ([(g) empty-g]
                  [(g brick5) (make-node g 'brick 5)]
                  [(g brickish) (make-node g 'brick (make-inexact 8))]
                  [(g eqn) (make-node g 'equation '3*8=24)]
                  [(g tag) (make-tag g '(has-operand 3) eqn)])
      (check-equal? (->number g brick5) 5)
      (check-equal? (->number g brickish) (inexact-number* 8) )
      (check-equal? (->number g tag) 3)))

  (test-case "require-tag* and seek-instance-of*"
    (let*-values ([(g) empty-g]
                  [(g eqn) (make-node g 'equation '3*8=24)]
                  [(g tag1) (make-tag g '(has-operand 3) eqn)]
                  [(g) (add-tag g '(has-operand 8) eqn)])
      (define req-exact3 (require-tag* 'has-operand 3))
      (define req-exact4 (require-tag* 'has-operand 4))
      (define req-exact8 (require-tag* 'has-operand 8))
      (define req-exact-badtag (require-tag* 'no-such-tag 3))
      (define req-inexact3 (require-tag* 'has-operand (make-inexact 3)))
      (define req-inexact4 (require-tag* 'has-operand (make-inexact 4)))
      (check-equal? (acceptability g req-exact3 eqn) 1.0)
      (check-equal? (acceptability g req-exact4 eqn) 0.0)
      (check-equal? (acceptability g req-exact8 eqn) 1.0)
      (check-equal? (acceptability g req-exact-badtag eqn) 0.0)
      (check-equal? (acceptability g req-inexact3 eqn) 1.0)
      (let ([a (acceptability g req-inexact4 eqn)])
        (check-true (< 0.1 a 0.9)))

      (define seek-equation (seek-instance-of* 'equation))
      (check-equal? (acceptability g seek-equation eqn) 1.0)
      (check-equal? (acceptability g seek-equation tag1) 0.0)

    ))
  )