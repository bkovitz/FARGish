; fb3.rkt -- Experimenting with code generation of local matchers, etc.

#lang debug typed/racket
(require typed/debug/report)

(require errortrace)
(require (for-syntax syntax/parse))
(require "typed-wheel.rkt" typed/racket/flonum)
(require "types.rkt" "fargish.rkt" "model.rkt" "trace.rkt" "support-typed.rkt")

(define-type SearchableNodesFunc (-> Graph (Listof Node)))

(: current-searchable-nodes-func : (Parameterof SearchableNodesFunc))
(define current-searchable-nodes-func (make-parameter all-nodes))

(: current-searchable-nodes : SearchableNodesFunc)
(define (current-searchable-nodes g)
  ((current-searchable-nodes-func) g))

(define MIN-SALIENCE : Salience 0.00001)

(: ok-salience : Salience -> Salience)
(define (ok-salience x)
  (if (< x MIN-SALIENCE) MIN-SALIENCE x))

(: choose-node-permutation-by-salience : Graph (Listof (Listof Node))
                                         -> (Listof Node))
(define (choose-node-permutation-by-salience g nodess)
  (let ([node->salience (g->node->salience g)]
        [perm->salience (λ ([nodes : (Listof Node)]) : Salience
                          (ok-salience
                            (foldl fl* 1.0 (map node->salience nodes))))]
        [perm (weighted-choice-by perm->salience nodess)])
    (cond
      [(void? perm) '()]
      [else perm])))

; In lieu of a FARGish spec
(: number : (U Integer Void) -> Attrs)
(define (number n)
  (hash 'class 'number
        'value n))

(: find-matching-nodes : Graph (-> Graph Node Any) -> (Listof Node))
(define (find-matching-nodes g pred)
  (filter/g g pred (current-searchable-nodes g)))

(: find-node-permutations : Graph (-> Graph Node Any) *
                            -> (Listof (Listof Node)))
(define (find-node-permutations g . preds)
  (let ([nodess : (Listof (Listof Node))
          (for/list ([pred preds])
            (find-matching-nodes g pred))])
    (apply cartesian-product nodess)))

(: choose-node-permutation : Graph (Listof (Listof Node)) -> (Listof Node))
;STUB: Just chooses the first
;(define (choose-node-permutation g permutations)
;  (cond
;    [(null? permutations) '()]
;    [else (car permutations)]))
(define choose-node-permutation choose-node-permutation-by-salience)

(define-syntax (bind-from-list stx)
  (syntax-parse stx
    [(_ ls:expr () body ...+)
     #'(begin body ...)]
    [(_ ls-expr:expr ([name1:id colon type1:expr]
                      [name*:id colon2 type*:expr] ...) body ...+)
     #'(let-values ([(ls) ls-expr])
         (cond
           [(null? ls) (let-values ([([name1 : type1]) (void)])
                         (bind-from-list '() ([name* : type*] ...)
                           body ...))]
           [else (let-values ([([name1 : type1]) (car ls)])
                   (bind-from-list (cdr ls) ([name* : type*] ...)
                     body ...))]))]))
     
(define-syntax (scout-for stx)
  (syntax-parse stx
    [(_ g-expr:expr ([name:id pred:expr] ...)
               aggregate-pred-expr:expr
       body:expr ...+)
     #'(let ([g : Graph g-expr]
             [node-permutations
               (for/list : (Listof (Listof Node))
                         ([perm (find-node-permutations g pred ...)]
                          #:when (bind-from-list perm ([name : MaybeNode] ...)
                                   aggregate-pred-expr))
                 perm)]
             [perm (choose-node-permutation g node-permutations)])
         (cond
           [(void? perm) g]
           [(null? perm) g]
           [else (bind-from-list perm ([name : MaybeNode] ...)
                   body ...)]))]))

(: of-class : Symbol -> (-> Graph Node Boolean))
(define (of-class class)
  (λ ([g : Graph] [node : Node])
    (node-is-a? g node class)))

(: manually : Graph -> Any)  ; TODO: -> Graph
(define (manually g)
  (scout-for g ([n1 (of-class 'number)]
                [n2 (of-class 'number)])
               (safe->? (value-of g n1) (value-of g n2))
    #R n1
    #R n2
    (displayln "FOUND IT")))

;; ======================================================================
;;
;; Making a procedure for a local matcher (codelet)
;;

; args:  (nodeid node-condition) ...
;        collective-condition
;        (new-nodeid make-expr) ...
(define (mk-codelet ??)
  (syntax-parse '()
    [(_ ...)
     #`(λ ([g : Graph]) : Graph
         (scout-for g ([nodeid node-condition] ...)
                      collective-condition
           (let*-values ([(g new-nodeid) make-expr] ...)
             g)))]))
           

(define init-g : Graph
  (let
    ([(g) (make-empty-graph empty-spec)]
     [g (add-node g (number 6))]
     [g (add-node g (number 6))]
     [g (add-node g (number 4))])
    g))

; Throwaway test code

(define g : Graph init-g)
(set! g g)

(manually g)
