; fb3.rkt -- Experimenting with code generation of local matchers, etc.

#lang debug typed/racket
(require typed/debug/report)

(require errortrace)
(require (for-syntax syntax/parse))
(require "typed-wheel.rkt")
(require "types.rkt" "fargish.rkt" "model.rkt" "trace.rkt" "support-typed.rkt")

(: all-unique? (All (a) (Listof a) -> Boolean))
(define (all-unique? xs)
  (let ([st (list->set xs)])
    (= (length xs) (set-count st))))

(: number : (U Integer Void) -> Attrs)
(define (number n)
  (hash 'class 'number
        'value n))

(: find-matching-nodes : Graph (-> Graph Node Any) -> (Listof Node))
(define (find-matching-nodes g pred)
  (filter/g g pred (all-nodes g)))

(: find-node-permutations : Graph (-> Graph Node Any) *
                            -> (Listof (Listof Node)))
(define (find-node-permutations g . preds)
  (let ([nodess : (Listof (Listof Node))
          (for/list ([pred preds])
            (find-matching-nodes g pred))])
    (apply cartesian-product nodess)))

;STUB: Just chooses the first
(: choose-node-permutation : Graph (Listof (Listof Node)) -> (Listof Node))
(define (choose-node-permutation g permutations)
  (cond
    [(null? permutations) '()]
    [else (car permutations)]))

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
     #'(let ([g g-expr]
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
