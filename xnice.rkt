; xnice.rkt -- Experimenting with making nice syntax for specifying a FARG model

#lang debug at-exp racket

(require (for-syntax racket/syntax) racket/syntax)
(require debug/repl describe)
(require (prefix-in g: "graph.rkt") (prefix-in g: "make-graph.rkt"))

(define empty-set (set))
(define empty-hash (hash))

#;(define-syntax (define/g stx)
  (syntax-case stx ()
    [(define/g (name g args ...) body0 body ...)
     (with-syntax ([name/g (format-id #'name "~a/g" #'name
                                      #:source #'name #:props #'name)])
       #'(begin
           (define (name g args ...) body0 body ...)
           (define (name/g args ...)
             (λ (g) (name g args ...)))))]))

;; A function whose first argument is a graph, and that returns one or more
;; values, the first of which is the updated graph.
(struct gfunc (f) #:property prop:procedure 0)

(define-syntax-rule (gλ args body0 body ...)
  (gfunc (λ args body0 body ...)))

;NEXT: Make the /g function a struct
(define-syntax (define/g stx)
  (syntax-case stx ()
    [(define/g (name g args ...) body0 body ...)
     (with-syntax ([name/g (format-id #'name "~a/g" #'name
                                      #:source #'name #:props #'name)])
       #'(begin
           (define name (gλ (g args ...) body0 body ...))
           (define (name/g args ...)
             (gλ (g) (name g args ...)))))]))

(struct is-a* (ancestors) #:prefab)
; ancestors: (Setof Symbol)

(define (is-a . args)
  (is-a* (list->set args)))

(struct by-ports* (from-port to-port) #:prefab)

(define by-ports by-ports*)

(struct links-into* (ctx-class by-portss) #:prefab)
;ctx-class : Symbol
;by-portss : (Listof by-ports)

(define (links-into ctx-class . by-portss)
  (links-into* ctx-class by-portss))

(struct nodeclass* (name ancestors links-intos) #:prefab)

(define (make-nodeclass name . elems)
  (for/fold ([ancestors empty-set]
             [links-intos empty-hash]
             #:result (nodeclass* name ancestors links-intos))
            ([elem elems])
    (match elem
      [(is-a* ancestors-)
       (values (set-union ancestors ancestors-) links-intos)]
      [(links-into* ctx by-portss)
       (values ancestors (hash-set links-intos ctx by-portss))])))

(define-syntax-rule (nodeclass name elems ...)
  (make-nodeclass (quote name) elems ...))

;; Single instance
;(define (tag-applies-to? (need source) . nodes)
;  (let ([pred (λ (node)
;                (no-neighbor-at-port?/g 'source node))])
;    (apply pred nodes)))
;
;;; Assuming only one condition for now
;(define (tag-applies-to? g tagclass . nodes)
;  (let ([condition?/g (apply (tagclass-condition tagclass) nodes)])
;    (condition?/g g)))
;
;(define (tag-applies-to?/g tagclass . nodes)
;  (apply (tagclass-condition tagclass) nodes))
;
;(define (tag-applies-to? g tagclass . nodes)
;  (let ([condition?/g (tagclass-condition tagclass)])
;    (apply condition?/g g nodes)))
;
;(define (tag-applies-to?/g tagclass . nodes)
;  (λ (g) (apply tag-applies-to? g tagclass nodes)))

(struct applies-to* (taggees conditions) #:prefab)

(struct taggee* (name of-classes by-portss) #:prefab)

(struct of-class* (class) #:prefab)

(define of-class of-class*)

(define (make-taggee name . taggee-infos)
  (define-values (of-classes by-portss)
    (for/fold ([of-classes '()] [by-portss '()])
              ([taggee-info taggee-infos])
      (cond
        [(of-class*? taggee-info)
         (values (cons taggee-info of-classes) by-portss)]
        [(by-ports*? taggee-info)
         (values of-classes (cons taggee-info by-portss))]
        [else (raise-arguments-error 'applies-to
                @~a{@taggee-info is neither of-class nor by-ports.})])))
  (taggee* name of-classes by-portss))

(define-syntax applies-to
  (syntax-rules (condition)
    [(applies-to ([taggee taggee-info ...] ...)
       (condition condition-expr) ...)
     (applies-to* (list (make-taggee 'taggee taggee-info ...) ...)
                  (list (make-condition-func taggee ...)))]))

; Makes a function that returns a g-func that returns true if the condition
; applies to the given nodes. TODO: Check preconditions from tag-infos.
(define-syntax make-condition-func
  (syntax-rules ()
    [(make-condition-func (taggee ...) body0 body ...)
     (let ([num-taggees (length (list taggee ...))]
           [make-pred/g (λ (taggee ...) body0 body ...)])
       (λ nodes
         (if (not (= num-taggees (length nodes)))
           (λ (g) #f)  ; tag can't apply if number of nodes is wrong
           (apply make-pred/g nodes))))]))

(struct farg-model-spec* (nodeclasses) #:prefab)
; nodeclasses: (Immutable-HashTable Any nodeclass)

(define (farg-model-spec . nodeclasses)
  (define ht (for/hash ([nodeclass nodeclasses])
               (values (nodeclass*-name nodeclass) nodeclass)))
  (farg-model-spec* ht))

(define spec
  (farg-model-spec
    (nodeclass ws)))

(define start-graph (struct-copy g:graph g:empty-graph [spec spec]))

(define/g (make-node g classname [value (void)])
  (define nodeclass (hash-ref (g:graph-spec g) classname
                              (λ ()
                                (raise-arguments-error 'make-node
                                  @~a{Undefined class name: @{classname}.}))))
  (cond
    [(void? value)
     (g:make-node g (make-hash 'class classname))]
    [else
     (g:make-node g (make-hash 'class classname 'value value))]))

(define g g:empty-graph)

(define (call-gfunc gfunc g . args)
  (call-with-values (λ () (apply gfunc g args))
    (λ vals vals)))

(define graph-eval
  (let ([ev (current-eval)])
    (λ (x)
      (writeln x)
      (define (fix-up stx)
        (syntax-case stx ()
          [(top f args ...)
           (begin (println "HERE") #R (gfunc? (graph-eval #'(#%top-interaction . f))))
           (with-syntax ([g (format-id #'f "g"
                                       #:source #'f #:props #'f)])
             #'(top f g args ...))]
          [x #'x]))
      (writeln (fix-up x))
      (ev (fix-up x)))))

;(define-syntax (~>graph! stx)
;  (syntax-case stx ()
;    [(~>graph! g)
;     #'((void))]
;    [(~>graph! g (f args ...) more ...)
;     (call-with-values (λ () (f g args ...))
;       (λ result
;         (match result
;           [`(,new-g)
;             (set! g new-g)
;             (~>graph! g more ...)]
;           [`(,new-g ,return-value)
;             (set! g new-g)

