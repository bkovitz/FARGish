; fargish1.rkt -- A little language for specifying FARG models

#lang debug at-exp racket

(require syntax/parse syntax/parse/define
         (for-syntax syntax/parse))
(require racket/hash)
(require "wheel.rkt")
(require rackunit)

(provide farg-model-spec
         nodeclass
         tagclass
         portclass
         args->node-attrs
         apply-class-attr
         realize-attrs

         as-member

         get-nodeclass-attr
         get-nodeclass*
         nodeclass-name
         nodeclass-is-a?
         get-raw-nodeclass-attr
         archetype-name

         get-portclass*
         get-portclass-attr

         (struct-out farg-model-spec*)
         (struct-out nodeclass*)
         (struct-out portclass*)
         (struct-out by-ports*)
         by-ports
         (struct-out links-into*)
         (struct-out applies-to*)
         (struct-out taggee-info*))

;; ======================================================================
;;
;; structs that compose a FARG model spec
;;

(struct farg-model-spec* (nodeclasses ancestors portclasses) #:prefab)
; nodeclasses: (Immutable-HashTable Symbol nodeclass*)
; ancestors: (Immutable-HashTable Symbol (Setof Symbol))
; portclasses: (Immutable-HashTable Symbol portclass*)

(struct nodeclass* (name class-attrs) #:prefab)
; class-attrs: (Immutable-HashTable Symbol Any)
; ancestors: (Setof Symbol)

(struct portclass* (name class-attrs) #:prefab)
; class-attrs: (Immutable-HashTable Symbol Any)

(struct by-ports* (from-port-label to-port-label) #:prefab)
; from-port-label: Symbol
; to-port-label: Symbol
(define by-ports by-ports*)

(struct links-into* (ctx-class by-portss) #:prefab)
;ctx-class : Symbol
;by-portss : (Listof by-ports)

(struct applies-to* (taggee-infos conditions) #:prefab)
; taggees: (List taggee-info*)
; conditions: (List cfunc)

(struct taggee-info* (name of-classes by-portss) #:prefab)
; name: Any
; of-classes: (Listof Symbol)
; by-portss: (List by-ports*)

;; ======================================================================
;;
;; Macros that define a nodeclass
;;

(define-syntax make-condition-func
  (syntax-rules ()
    [(make-condition-func (taggee ...) body0 body ...)
     (let ([num-taggees (length (list 'taggee ...))]
           [make-pred/g (λ (taggee ...) body0 body ...)])
       (λ nodes
         (if (not (= num-taggees (length nodes)))
           (λ (g) #f)  ; tag can't apply if number of nodes is wrong
           (apply make-pred/g nodes))))]))

(define-syntax (nodeclass-body stx)
  (define-syntax-class name-and-args
    #:description "nodeclass name or name with arguments"
    #:attributes [(name 0) (arg 1)]
    (pattern name:id #:with (arg ...) #'())
    (pattern (name:id arg:id ...)))

  (define-splicing-syntax-class li-body
    #:description "body of links-into"
    #:attributes [(ctx 0) (by-ports 1)]
    (pattern (~seq ctx:expr by-ports:expr ...)))

  (define-splicing-syntax-class taggee-body
    #:description "taggee"
    #:datum-literals [of-class]
    ;#:attributes [(name 0) (of-class-expr 2) (by-ports 1)]
    #:auto-nested-attributes
    (pattern (~seq name:id
                   (~alt (of-class ~! of-class-expr:expr ...)
                         by-ports:expr) ...)))

  ;TODO Allow any number of conditions: 0 or more. If 0, then always pass.
  (define-splicing-syntax-class a2-body
    #:description "body of applies-to"
    #:datum-literals [condition]
    ;#:attributes [(taggee 1) (condition-expr 1)]
    #:auto-nested-attributes
    (pattern (~seq ([taggee:taggee-body] ...+)
               (condition condition-expr:expr ...))))

  (define-splicing-syntax-class nodeclass-elems
    #:description "nodeclass elements"
    #:datum-literals [is-a archetype name value links-into applies-to]
;    #:attributes [(parent-expr 2) (archetype-expr 2) (name-expr 1)
;                  (value-expr 1) (li-expr 1)]
    #:auto-nested-attributes
    (pattern (~seq (~alt (is-a ~! parent-expr:expr ...+)
                         (archetype ~! archetype-expr:expr ...+)
                         (name ~! name-expr:expr)
                         (value ~! value-expr:expr)
                         (links-into ~! li-expr:li-body)
                         (applies-to ~! a2-expr:a2-body)
                         ) ...)))

  (syntax-parse stx
    [(_ nm:name-and-args elems:nodeclass-elems)
     #`(hash 'name 'nm.name
             'args '(nm.arg ...)
             'parents (list elems.parent-expr ... ...)
             'display-name (list (λ (nm.arg ...) elems.name-expr) ...)
             'archetype-names (list (λ (nm.arg ...)
                                      elems.archetype-expr) ... ...)
             'default-archetype-name (λ (nm.arg ...)
                                       (list 'nm.name nm.arg ...))
             'value (list (λ (nm.arg ...) elems.value-expr) ...)
             'links-into
               (list (λ (nm.arg ...)
                       (links-into* elems.li-expr.ctx
                                    (list elems.li-expr.by-ports ...))) ...)
             'applies-to
               (list (λ (nm.arg ...)
                       (applies-to*
                         (list (taggee-info*
                                 'elems.a2-expr.taggee.name
                                 (list elems.a2-expr.taggee.of-class-expr
                                       ... ...)
                                 (list elems.a2-expr.taggee.by-ports ...))
                                 ...)
                         (list (make-condition-func
                                 (elems.a2-expr.taggee.name ...)
                                 elems.a2-expr.condition-expr ...) ...)))
                     ...))]))

(define-syntax (portclass-body stx)
  (define-splicing-syntax-class portclass-elems
    #:description "portclass elements"
    #:datum-literals [max-neighbors]
    #:attributes [(max-neighbors-expr 1)]
    (pattern (~seq (max-neighbors ~! max-neighbors-expr:expr) ...)))

  (syntax-parse stx
    [(_ nm:id elems:portclass-elems)
     #`(hash 'name 'nm
             'max-neighbors (list elems.max-neighbors-expr ...))]))

(define-syntax-rule (nodeclass body ...)
  (make-nodeclass (nodeclass-body body ...)))

(define-syntax-rule (tagclass body ...)
  (let ([class-attrs (nodeclass-body body ...)])
    (make-nodeclass (hash-set class-attrs 'tag? #t))))

(define-syntax-rule (portclass body ...)
  (make-portclass (portclass-body body ...)))

;; ======================================================================
;;
;; Ancillary functions for the nodeclass and portclass macros
;;

(define (set-to-last-defined attrs key fk)
  (hash-ref/sk attrs key
    (λ (v) (cond
             [(null? v) (fk)]
             [else (hash-set attrs key (last v))]))
    fk))

;TODO Verify that all the by-portss are by-ports* instances.
(define (make-nodeclass class-attrs)
  (let* ([name (hash-ref class-attrs 'name)]
         [class-attrs (set-to-last-defined class-attrs 'value
                        (λ () (hash-remove class-attrs 'value)))]
         [class-attrs (set-to-last-defined class-attrs 'display-name
                        (λ () (hash-remove class-attrs 'display-name)))]
         [class-attrs (add-default-class-attrs class-attrs)]
         [class-attrs (hash-remove class-attrs 'default-archetype-name)])
    (nodeclass* name
                class-attrs)))

(define (make-portclass class-attrs)
  (let* ([name (hash-ref class-attrs 'name)]
         [class-attrs (set-to-last-defined class-attrs 'max-neighbors
                        (λ ()
                          (hash-set class-attrs 'max-neighbors +inf.0)))])
    (portclass* name class-attrs)))

(define (add-default-class-attrs attrs)
  (hash-ref/sk attrs 'tag? (λ (flag) (if flag
                                         (add-default-tag-attrs attrs)
                                         attrs))
                           attrs))

(define (add-default-tag-attrs attrs)
  (define archetype-names (hash-ref attrs 'archetype-names '()))
  (if (null? archetype-names)
    (hash-set attrs
              'archetype-names
              (list (hash-ref attrs 'default-archetype-name)))
    attrs))

;; ======================================================================
;;
;; Functions to access elements of a nodeclass
;;

; value can be a procedure, a value, or a list of procedures and/or values.
; Whenever a procedure is found, we pass it args.
(define (apply-class-attr value args)
  (define (apply-f f)
    (cond
      [(procedure? f) (apply f args)]
      [else f]))
  (cond
    [(void? value) (void)]
    [(pair? value) (for/list ([f value])
                    (apply-f f))]
    [else (apply-f value)]))

(define (get-raw-nodeclass-attr nodeclass key)
  (let ([class-attrs (nodeclass*-class-attrs nodeclass)])
    (hash-ref class-attrs key (void))))

(define (get-nodeclass-attr nodeclass key [args '()])
  (let* ([class-attrs (nodeclass*-class-attrs nodeclass)] ;TODO OAOO prev
         [value (hash-ref class-attrs key (void))])
    (apply-class-attr value args)))

(define (class-attr->node-attr class-attrs node-attrs args class-key node-key)
  (let* ([value (hash-ref class-attrs class-key (void))]
         [v (apply-class-attr value args)])
    (if (void? v)
      node-attrs
      (hash-set node-attrs node-key v))))

(define (args->node-attrs nodeclass args)
  (let* ([class-attrs (nodeclass*-class-attrs nodeclass)]
         [attrs (hash 'args args
                      'class (nodeclass*-name nodeclass))]
         [attrs (class-attr->node-attr class-attrs attrs args
                                       'value 'value)]
         [attrs (class-attr->node-attr class-attrs attrs args
                                       'display-name 'name)]
         [attrs (class-attr->node-attr class-attrs attrs args
                                       'archetype-names 'archetype-names)]
         [attrs (class-attr->node-attr class-attrs attrs args
                                       'tag? 'tag?)])
    attrs))

; Returns nodeclass* nc with all class-attrs updated so that procedures
; that need to be passed args are called and replaced with their results.
(define (realize-attrs nc args)
  (define realized-attrs (for/hash ([(k v) (nodeclass*-class-attrs nc)])
                           (values k (apply-class-attr v args))))
  (struct-copy nodeclass* nc
    [class-attrs realized-attrs]))

;; ======================================================================
;;
;; Functions to access elements of a portclass
;;

(define (get-portclass-attr portclass key [default (void)])
  (hash-ref (portclass*-class-attrs portclass) key default))

;; ======================================================================
;;
;; Unit tests
;;

(module+ test
  (test-case "nodeclass with explicit name and value"
    (define number
      (nodeclass (number n)
        (is-a 'parent)
        (name n)
        (links-into 'ctx 'by-ports)
        (archetype 'a)
        (value n)))

    (define attrs (args->node-attrs number (list 5)))
    (check-equal? (hash-ref attrs 'class)
                  'number)
    (check-equal? (hash-ref attrs 'value)
                  5)
    (check-equal? (hash-ref attrs 'name)
                  5)

    (check-equal? (hash-ref attrs 'tag? (void))
                  (void)))
  
  (test-case "nodeclass with no name and no value"
    (define number
      (nodeclass (number n)
        (is-a 'parent)
        (links-into 'ctx 'by-ports)
        (archetype 'a)))

    (define attrs (args->node-attrs number (list 5)))
    (check-equal? (hash-ref attrs 'value (void))
                  (void))
    (check-equal? (hash-ref attrs 'name (void))
                  (void)))  ; farg-model-spec will assign a display-name

  (test-case "nodeclass with multiple names and values: the last one prevails"
    (define number
      (nodeclass (number n)
        (is-a 'parent)
        (name (list 'first-name n))
        (links-into 'ctx 'by-ports)
        (value (list 'first n))
        (archetype 'a)
        (value (list 'second n))
        (name (list 'second-name n))))

    (define attrs (args->node-attrs number (list 5)))
    (check-equal? (hash-ref attrs 'value (void))
                  (list 'second 5))
    (check-equal? (hash-ref attrs 'name)
                  '(second-name 5)))

  (test-case "three archetype-names in two clauses"
    (define number
      (nodeclass (number n)
        (is-a 'parent)
        (name n)
        (links-into 'ctx 'by-ports)
        (archetype 'a 'b)
        (archetype 'c)
        (value n)))

    (define attrs (args->node-attrs number (list 5)))
    (check-equal? (hash-ref attrs 'archetype-names (void))
                  '(a b c)))

  (test-case "get-nodeclass-attr and links-into"
    (define number
      (nodeclass (number n)
        (is-a 'parent)
        (name n)
        (links-into 'ctx (by-ports 'members 'member-of))
        (value n)))
    (check-equal? (get-nodeclass-attr number 'name (list 5))
                  'number)
    (check-equal? (get-nodeclass-attr number 'args)
                  '(n))
    (check-equal? (get-nodeclass-attr number 'links-into (list 5))
                  (list (links-into* 'ctx
                                     (list (by-ports 'members 'member-of))))))

  (test-case "tagclass and applies-to"
    (define tag
      (tagclass (tag n)
        (is-a 'parent)
        (name n)
        (links-into 'ctx (by-ports 'members 'member-of))
        (applies-to ([node (of-class 'number) (by-ports 'tagged 'tags)])
          (condition (λ (g) (equal? n node))))
        (value n)))

    (define attrs (args->node-attrs tag (list 5)))
    (check-true (hash-ref attrs 'tag?))

    (define applies-tos (get-nodeclass-attr tag 'applies-to (list 5)))
    ; (Listof applies-to*) built for n == 5

    (define cfunc (car (applies-to*-conditions (car applies-tos))))
    ; The 'condition' func from above, with n == 5

    (define gfunc4 (cfunc 4))
    ; The 'condition' func, now with n == 5 and node == 4

    (check-false (gfunc4 'graph))  ; In real life, you'd pass a real graph here

    (define gfunc5 (cfunc 5))
    ; The 'condition' func, now with n == 5 and node == 5

    (check-true (gfunc5 'graph))))  ; In real life, you'd pass a real graph here

;; ======================================================================
;;
;; farg-model-spec
;;

(define as-member (by-ports 'members 'member-of))

(define (archetype-name value)
  (string->symbol (string-append "archetype-"
    (cond
      [(pair? value) (string-join (map ~a value) "-")]
      [else (~a value)]))))

(define predefined-nodeclasses
  (list
    (nodeclass (archetype value)
      (links-into 'slipnet (by-ports 'archetypes 'slipnet) as-member)
      (name (archetype-name value)))
    (nodeclass slipnet
      (is-a 'ctx))
    (nodeclass ws  ; workspace
      (is-a 'ctx))
    (nodeclass ctx)))

(define (farg-model-spec . classes)
  (let*-values ([(classes) (append predefined-nodeclasses classes)]
                [(ht) (hash-by type-of classes)]
                [(ht-nodeclasses ht-ancestors)
                   (set-up-nodeclasses (hash-ref ht 'nodeclass '()))]
                [(ht-portclasses)
                   (set-up-portclasses (hash-ref ht 'portclass '()))])
    (farg-model-spec* ht-nodeclasses ht-ancestors ht-portclasses)))

(define (type-of class-struct)
  (cond
    [(nodeclass*? class-struct) 'nodeclass]
    [(portclass*? class-struct) 'portclass]
    [else (raise-arguments-error 'farg-model-spec
            @~a{@class-struct is neither a nodeclass* nor a portclass*.})]))

(define (set-up-nodeclasses nodeclasses)
  (let* ([ht-nodeclasses (for/hash ([nodeclass nodeclasses])
                           (values (nodeclass*-name nodeclass) nodeclass))]
         [ht-ancestors (make-ancestors-table ht-nodeclasses)]
         [ht-nodeclasses (for/hash ([(name nc) (in-hash ht-nodeclasses)])
                           (values name
                                   (supply-default-class-attrs
                                     (inherit-attrs name ht-nodeclasses))))])
    (values ht-nodeclasses ht-ancestors)))

(define (supply-default-class-attrs nodeclass)
  (let* ([class-attrs (nodeclass*-class-attrs nodeclass)]
         [class-attrs (hash-update class-attrs 'display-name
                        (λ (v) (if (null? v)
                                 (nodeclass*-name nodeclass)
                                 v))
                        (λ () (nodeclass*-name nodeclass)))])
    (struct-copy nodeclass* nodeclass
                 [class-attrs class-attrs])))

(define (set-up-portclasses portclasses)
  (for/hash ([portclass portclasses])
    (values (portclass*-name portclass) portclass)))

;; ======================================================================
;;
;; Functions to access a farg-model-spec*
;;

(define (get-nodeclass* spec nodeclass-or-name)
  (cond
    [(nodeclass*? nodeclass-or-name) nodeclass-or-name]
    [else (hash-ref (farg-model-spec*-nodeclasses spec)
                    nodeclass-or-name
                    (void))]))

(define (nodeclass-name nodeclass-or-name)
  (if (nodeclass*? nodeclass-or-name)
    (nodeclass*-name nodeclass-or-name)
    nodeclass-or-name))

(define (nodeclass-is-a? spec nodeclass ancestor)
  (let ([child-name (nodeclass-name nodeclass)]
        [ancestor-name (nodeclass-name ancestor)])
    (hash-ref/sk (farg-model-spec*-ancestors spec) child-name
      (λ (ancestors) (set-member? ancestors ancestor-name))
      #f)))

(define (get-portclass* spec portclass-or-name)
  (cond
    [(portclass*? portclass-or-name) portclass-or-name]
    [else (hash-ref (farg-model-spec*-portclasses spec)
                    portclass-or-name
                    (void))]))

;; ======================================================================
;;
;; Ancillary functions for inheritance
;;

(define (make-ancestors-table ht-nodeclasses)
  (define (all-ancestors-of name)
    (let recur ([name name] [result (set)] [already-seen (set name)])
      (hash-ref/sk ht-nodeclasses name
        (λ (nc)
          (let* ([parents (set-subtract (list->set (nodeclass-parents nc))
                                        already-seen)]
                 [result (set-union result parents)]
                 [already-seen (set-union already-seen parents)])
            (if (set-empty? parents)
              result
              (apply set-union (map
                                 (λ (parent)
                                   (recur parent result already-seen))
                                 (set->list parents))))))
        result)))
  (for/hash ([name (hash-keys ht-nodeclasses)])
    (values name (set-add (all-ancestors-of name) name))))

(define (nodeclass-parents nodeclass)
  (get-nodeclass-attr nodeclass 'parents))

; Returns list of ancestors with "oldest" ancestor first. name is the last
; element of the result. Parents at one level occur in reverse order: the
; first parent appears last in the list. (Breadth-first traversal.)
(define (ancestors-in-descending-order name ht-nodeclasses)
  (define (recur result pending-parents)
    (cond
      [(null? pending-parents) result]
      [(member (car pending-parents) result)
       (recur result (cdr pending-parents))]
      [else (let* ([parent (car pending-parents)]
                   [nodeclass (hash-ref ht-nodeclasses parent (void))]
                   [grandparents (if (void? nodeclass)
                                   '()
                                   (nodeclass-parents nodeclass))])
              (recur (cons parent result)
                     (remove-duplicates (append (cdr pending-parents)
                                                grandparents))))]))
  (recur '() (list name)))

(define (ancestors-in-ascending-order name ht-nodeclasses)
  (reverse (ancestors-in-descending-order name ht-nodeclasses)))

(define (combine-attrs key ancestor-value child-value)
  (case key
    [(archetype-names applies-to links-into)
     (append child-value ancestor-value)]
    [(parents args)
     child-value]
    [else (if (or (null? child-value) (void? child-value))
            ancestor-value
            child-value)]))

(define (inherit-attrs nodeclass-name ht-nodeclasses)
  (let* ([nodeclass (hash-ref ht-nodeclasses nodeclass-name)]
         [ancestor-names (ancestors-in-descending-order nodeclass-name
                                                        ht-nodeclasses)]
         [ancestor-attrs (for/list ([ancestor-name ancestor-names])
                                   (define ancestor (hash-ref ht-nodeclasses
                                                              ancestor-name
                                                              #f))
                                   (if ancestor
                                     (nodeclass*-class-attrs ancestor)
                                     empty-hash))]
         [new-attrs
           (apply hash-union ancestor-attrs
                             #:combine/key combine-attrs)])
    (struct-copy nodeclass* nodeclass
                 [class-attrs new-attrs])))

;; ======================================================================
;;
;; Unit tests for inheritance
;;

(module+ test
  (test-case "class-attr inheritance: archetype-names"
    (define spec
      (farg-model-spec
        (nodeclass A
          (archetype 'A))
        (nodeclass B
          (is-a 'A)
          (archetype 'B))
        (nodeclass C
          (is-a 'B)
          (archetype 'C))))

    (define A (get-nodeclass* spec 'A))
    (define B (get-nodeclass* spec 'B))

    (check-true (nodeclass-is-a? spec 'B 'A))
    (check-false (nodeclass-is-a? spec 'A 'B))
    (check-true (nodeclass-is-a? spec 'A 'A))
    (check-true (nodeclass-is-a? spec 'B 'B))

    (check-equal? '(A) (get-nodeclass-attr A 'archetype-names))
    (check-equal? '(B A) (get-nodeclass-attr B 'archetype-names)))

  ;TODO Map args appropriately when child and parent have different args,
  ;or throw exception in farg-model-spec if it can't be done.
  
  (test-case "class-attr inheritance: name and value"
    (define spec
      (farg-model-spec
        (nodeclass (number n)
          (name n)
          (value n))
        (nodeclass (brick n)
          (is-a 'number))
        (nodeclass empty)))

    (define number (get-nodeclass* spec 'number))
    (define brick (get-nodeclass* spec 'brick))
    (define empty (get-nodeclass* spec 'empty))

    (define n7-attrs (args->node-attrs number (list 7)))
    (define b5-attrs (args->node-attrs brick (list 5)))
    (define e-attrs (args->node-attrs empty '()))

    (check-equal? (hash-ref n7-attrs 'name) 7)
    (check-equal? (hash-ref n7-attrs 'value) 7)

    (check-equal? (hash-ref b5-attrs 'name) 5)
    (check-equal? (hash-ref b5-attrs 'value) 5)

    (check-equal? (hash-ref e-attrs 'name) 'empty)
    (check-equal? (hash-ref e-attrs 'value (void)) (void))))

;; ======================================================================
;;
;; Unit tests for port classes
;;

(module+ test
  (define spec
    (farg-model-spec
      (nodeclass (number n)
        (name n)
        (value n))
      (nodeclass +)
      (portclass source
        (max-neighbors 1))
      (portclass result
        (max-neighbors 1))
      (portclass operands)
      #;(can-link 'source 'result)
      #;(can-link 'result 'operands)))

  (test-case "portclasses"
    (define source (get-portclass* spec 'source))
    (define operands (get-portclass* spec 'operands))

    (check-equal? (get-portclass-attr source 'name) 'source)

    (check-equal? (get-portclass-attr source 'max-neighbors) 1)
    (check-equal? (get-portclass-attr operands 'max-neighbors) +inf.0)))
