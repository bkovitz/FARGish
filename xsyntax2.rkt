#lang debug at-exp racket

;(define-syntax nodeclass
;  (nodeclass (args ...) elems ...)
;
;  (λ (args ...) elems ...)

(require syntax/parse syntax/parse/define
         (for-syntax syntax/parse))
(require "wheel.rkt")
(require rackunit)

(define-singleton placeholder)

(struct nodeclass* (name class-attrs ancestors) #:prefab)

(struct by-ports* (from-port-label to-port-label) #:prefab)
(define by-ports by-ports*)

(struct links-into* (ctx-class by-portss) #:prefab)
;ctx-class : Symbol
;by-portss : (Listof by-ports)

(define-syntax (nodeclass stx)
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
    #:attributes [(name 0) (of-class-expr 2) (by-ports 1)]
    (pattern (~seq name:id
                   (~alt (of-class ~! of-class-expr:expr ...)
                         by-ports:expr) ...)))

;  (define-splicing-syntax-class applies-to-body
;    #:description "body of applies-to"
;    #:attributes [(taggee 1) (condition 1)]
;    (pattern (~seq ([taggee:taggee-body] ...)
;               (condition condition-expr:expr ...+))))

  (define-splicing-syntax-class nodeclass-elems
    #:description "nodeclass elements"
    #:datum-literals [is-a archetype name value links-into]
;    #:attributes [(parent-expr 2) (archetype-expr 2) (name-expr 1)
;                  (value-expr 1) (li-expr 1)]
    #:auto-nested-attributes
    (pattern (~seq (~alt (is-a ~! parent-expr:expr ...+)
                         (archetype ~! archetype-expr:expr ...+)
                         (name ~! name-expr:expr)
                         (value ~! value-expr:expr)
                         (links-into ~! li-expr:li-body)
                         ) ...)))

  (syntax-parse stx
    ;[(nodeclass (name:id arg:id ...) elems:nodeclass-elems)
    [(nodeclass nm:name-and-args elems:nodeclass-elems)
     #`(make-nodeclass
         (hash 'name 'nm.name
               'args '(nm.arg ...)
               'parents (list elems.parent-expr ... ...)
               'display-name (list (λ (nm.arg ...) elems.name-expr) ...)
               'archetype-names (list (λ (nm.arg ...)
                                        elems.archetype-expr) ... ...)
               'value (list (λ (nm.arg ...) elems.value-expr) ...)
               'links-into
                 (list (λ (nm.arg ...)
                         (links-into* elems.li-expr.ctx
                                      (list elems.li-expr.by-ports ...))) ...)
               ))]))

;(define (make-nodeclass class-attrs)
;  (let ([class-attrs (
;  (define class-attrs
;    (hash 'name (hash-ref ht 'name)
;          'args (hash-ref ht 'args)
;          'parents (hash-ref ht 'is-a)
;          'archetypes (hash-ref

(define (set-to-last-defined attrs key fk)
  (hash-ref/sk attrs key
    (λ (v) (cond
             [(null? v) (fk)]
             [else (hash-set attrs key (last v))]))
    fk))

(define (make-nodeclass class-attrs)
  (let* ([name (hash-ref class-attrs 'name)]
         [class-attrs (set-to-last-defined class-attrs 'value
                        (λ () (hash-remove class-attrs 'value)))]
         [class-attrs (set-to-last-defined class-attrs 'display-name
                        (λ () (hash-set class-attrs 'display-name
                                (λ _ name))))])

    (nodeclass* (hash-ref class-attrs 'name)
                class-attrs
                placeholder)))

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

(define (get-nodeclass-attr nodeclass key [args '()])
  (let* ([class-attrs (nodeclass*-class-attrs nodeclass)]
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
         )
    attrs))

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
    )
  
  (test-case "nodeclass with no name and no value"
    (define number
      (nodeclass (number n)
        (is-a 'parent)
        (links-into 'ctx 'by-ports)
        (archetype 'a)))

    (define attrs (args->node-attrs number (list 5)))
    (check-equal? (hash-ref attrs 'value (void))
                  (void))
    (check-equal? (hash-ref attrs 'name)
                  'number)
    )

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
  )



(define number
  (nodeclass (number n)
    (is-a 'parent)
    (name n)
    (links-into 'ctx (by-ports 'members 'member-of))
    (value n)))

number
(get-nodeclass-attr number 'name (list 5))
(get-nodeclass-attr number 'args)
(get-nodeclass-attr number 'links-into (list 5))
