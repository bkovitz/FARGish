; completion1.rkt -- Hackish way to "complete" a partial instance of one
;                    ctx in another

#lang debug at-exp racket

(require errortrace)
(require "wheel.rkt"
         "xsusp3.rkt"
         "model1.rkt"
         "shorthand.rkt"
         (prefix-in f: "fargish1.rkt")
         (only-in "fargish1.rkt"
           farg-model-spec nodeclass tagclass)
         (prefix-in g: "graph1.rkt")
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo))
(require racket/list)
(require rackunit racket/pretty describe)

(provide complete-partial-instance-in best-completion-actions)

;; ======================================================================
;;
;; complete-partial-instance-in
;;

(define (complete-partial-instance-in g from-ctx to-ctx)
  (cdr (best-completion/g-actions g from-ctx to-ctx)))

(define (best-completion-actions g from-ctx to-ctx)
  (car (best-completion/g-actions g from-ctx to-ctx)))

(define (best-completion/g-actions g from-ctx to-ctx)
  (define actions->g
    (for/fold ([ht empty-hash])
              ([bdx-actions (all-possible-completions g from-ctx to-ctx)])
      (with-handlers ([cant-make-edge? (const ht)])
        (define g-with-completion (try-bdx-actions g
                                    from-ctx to-ctx bdx-actions))
        (hash-set ht bdx-actions g-with-completion))))
  (if (hash-empty? actions->g)
    (tag-failed g from-ctx)
    (best-completion actions->g)))

(define (all-possible-completions g from-ctx to-ctx)
  (filter-out-invalid-completions
    (apply cartesian-product
      (for/list ([from-node (members-of g from-ctx)])
        (build/bind-actions g from-ctx from-node to-ctx)))))

; The completion with the fewest builds wins
(define (best-completion actions->g)
  (argmin (λ (ag) (count build? (car ag)))
          (hash->list actions->g)))

(define (tag-failed g node)
  g) ;STUB

; throws 'cant-make-edge
(define (try-bdx-actions g0 from-ctx to-ctx bdx-actions)
  (let*-values ([(from-edges) (required-edges-in-ctx g0 from-ctx)]
                [(g ht-bdx) (do-bdx-actions g0 from-ctx to-ctx bdx-actions)])
    (build-corresponding-edges g from-ctx from-edges to-ctx ht-bdx)))

; throws 'cant-make-edge
(define (build-corresponding-edges g from-ctx from-edges to-ctx ht-bdx)
  (for/fold ([g g])
            ([from-edge from-edges])
    (match-define `(,from-port1 ,from-port2) (set->list from-edge))
    (match-define `(,from-node1 ,port-label1) from-port1)
    (match-define `(,from-node2 ,port-label2) from-port2)
    (define to-node1 (hash-ref ht-bdx from-node1))
    (define to-node2 (hash-ref ht-bdx from-node2))
    (define to-port1 `(,to-node1 ,port-label1))
    (define to-port2 `(,to-node2 ,port-label2))
    (define to-hop `(,to-port1 ,to-port2))
    (cond
      [(g:has-edge? g to-hop)
       g]
      [(and (open-port? g from-port1 to-port1)
            (open-port? g from-port2 to-port2))
       (add-edge g to-hop)]
      [else (raise 'cant-make-edge)])))

(define (open-port? g from-port to-port)
  ; Limiting the number of neighbors at the to-port to the number at the
  ; from-port (the port in the archetypal group) is something of a hack.
  (define max-n (min (set-count (g:port->neighboring-ports g from-port))
                     (max-neighbors-of-port g to-port)))
  (< (set-count (g:port->neighboring-ports g to-port))
     max-n))

(define (do-bdx-actions g from-ctx to-ctx bdx-actions)
  (for/fold ([g g] [ht-bdx empty-hash])
            ([bdx-action bdx-actions])
    (match bdx-action
      [`(bind ,from-node ,to-node)
        (values g (hash-set ht-bdx from-node to-node))]
      [`(build ,from-node)
        (let*-values ([(g to-id) (apply make-node/in g
                                                     to-ctx
                                                     (class-of g from-node)
                                                     (args-of g from-node))])
          (values g (hash-set ht-bdx from-node to-id)))])))

(define (required-edges-in-ctx g ctx)
  (for*/set ([node (members-of g ctx)]
             [hop (g:node->incident-hops g node)]
             #:when (member-of? g ctx (g:other-node hop)))
    (list->set hop)))

(define (filter-out-invalid-completions completions)
  (for/list ([completion completions]
             #:when (and (not (all-binds? completion))
                         (not (all-builds? completion))
                         (not (multiple-binds-to-same-bindee? completion))))
    completion))

(define (build/bind-actions g from-ctx from-node to-ctx)
  `((build ,from-node) ,@(all-reasonable-bindings-from-to g
                           from-ctx from-node to-ctx)))

(define (all-reasonable-bindings-from-to g from-ctx from-node to-ctx)
  (for/list ([to-node (members-of g to-ctx)]
              #:when (superficial-match? g from-node to-node))
    `(bind ,from-node ,to-node)))

(define (superficial-match? g from-node to-node)
  (and (node-is-a? g to-node (class-of g from-node))
       (equal? (value-of g from-node) (value-of g to-node))))

(define (cant-make-edge? x)
  (eq? 'cant-make-edge x))

(define (bind? x)
  (and (pair? x) (eq? 'bind (car x))))

(define (build? x)
  (and (pair? x) (eq? 'build (car x))))

(define (all-binds? completion)
  (andmap bind? completion))

(define (all-builds? completion)
  (andmap build? completion))

(define (multiple-binds-to-same-bindee? completion)
  (let/cc break
    (for/fold ([st empty-set] #:result #f)
              ([action completion])
      (match action
        [`(bind ,from-node ,to-node)
          (when (set-member? st to-node)
            (break #t))
          (set-add st to-node)]
        [else st]))))

;; ======================================================================
;;
;; Unit tests
;;

(module+ test
  (test-case "complete-partial-instance-in"
    (define spec
      (farg-model-spec
        (nodeclass (number n)
          (value n)
          (name n))
        (nodeclass (brick n)
          (is-a 'number))
        (nodeclass +)
        (nodeclass equation
          (is-a 'ctx))))

    (define g (void))
    (set! g
      (make-graph spec
        '(ws)
        '(:in ws
           (:let ([15 (number 15)]
                  [4 (number 4)]
                  [5 (number 5)]
                  [6 (number 6)]
                  [+ (+)]
                  [9 (number 9)])
             (:edge 4 result + operands)
             (:edge 5 result + operands)
             (:edge + result 9 source)))
        '(:in (slipnet)
           (:in (equation)
             (:let ([15 (number 15)]
                    [+ (+)]
                    [9 (number 9)]
                    [6 (number 6)])
               (:edge 15 source + result)
               (:edge 9 result + operands)
               (:edge 6 result + operands))))))

    (gdo complete-partial-instance-in 'equation 'ws)

    ; Completion should build only a new +, and link it to make 9 + 6 = 15.

    (check-equal? (list->set (ids->names g (members-of g 'ws)))
                  (list->set '(4 5 6 + 9 + 15)))))
