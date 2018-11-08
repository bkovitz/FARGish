; fargish1.rkt -- A little language for specifying FARG models

#lang debug at-exp racket

(require racket/hash)
(require (prefix-in g: "graph1.rkt") (only-in "graph1.rkt" define/g gdo))
(require "wheel.rkt")
(require (for-syntax racket/syntax syntax/parse racket)
         (for-template racket)
         racket/syntax)
(require rackunit debug/repl describe)

;; ======================================================================
;;
;; Structs that hold a FARG model specification and its elements
;;

(struct farg-model-spec* (nodeclasses ancestors) #:prefab)
; nodeclasses: (Immutable-HashTable Symbol nodeclass*)
; ancestors: (Immutable-HashTable Symbol (Setof Symbol))

(struct nodeclass* (name class-attrs attr-ctors ancestors) #:prefab)
; attr-ctors: (Hashof Symbol Func-of-node-args)

(struct is-a* (parents) #:prefab)
; parents: (Listof Symbol)

(struct by-ports* (from-port-label to-port-label) #:prefab)

(struct links-into* (ctx-class by-portss) #:prefab)
;ctx-class : Symbol
;by-portss : (Listof by-ports)

(struct default-attrs* (attrs))
;attrs: (Hash Any Any)

(struct applies-to* (taggee-infos conditions) #:prefab)
; taggees: (List taggee-info*)
; conditions: (List cfunc)

;TODO taggee-info*
(struct taggee-info* (name of-classes by-portss) #:prefab)
; name: Any
; of-classes: (List of-class*)
; by-portss: (List by-ports*)

(struct of-class* (class) #:prefab)
; class: Any

;; ======================================================================
;;
;; Functions and macros to create elements of FARG model specifications
;;
;; A definition of a FARG model specification should consist entirely of
;; invocations of these functions and macros.
;;

#;(define (farg-model-spec . nodeclasses)
  (let* ([ht-nodeclasses (for/hash ([nodeclass nodeclasses])
                           (values (nodeclass*-name nodeclass) nodeclass))])
    (farg-model-spec* ht-nodeclasses 'STUB)))

(define farg-model-spec list)

(define-syntax (nodeclass stx)
  (define-syntax-class archetype
    (pattern ((~literal archetype) a:expr ...)))

  (syntax-parse stx
    [(nodeclass (name:id arg:id ...) atype:archetype ...)
     #'(make-nodeclass 'name '(arg ...) '(atype ...))])
  )

(define make-nodeclass list)

(define (make-node-attrs nodeclass args)
  (for/fold ([attrs (hash 'class (nodeclass*-name nodeclass))])
            ([(attr-name attr-ctor) (nodeclass*-attr-ctors nodeclass)])
    (hash-set attrs attr-name (apply attr-ctor args))))

;; ======================================================================
;;
;; Unit test that shows how all the elements fit together
;;

#;(module+ test
  (test-case "spec basics"
    (define spec
      (farg-model-spec
        (nodeclass (number n)
          (archetype no-archetype))))
    'VOID
))


(define spec
      (farg-model-spec
        (nodeclass (number n)
          (archetype n))))
