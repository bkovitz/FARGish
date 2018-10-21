; xgnice.rkt -- Throwaway code: experimenting with ways to make operating
;               on graphs a little nicer

#lang debug at-exp racket

(require (for-syntax racket/syntax) racket/syntax)
(require debug/repl describe)
(require (prefix-in g: "graph.rkt") (prefix-in g: "make-graph.rkt"))

;; A function whose first argument is a graph, and that returns one or more
;; values, the first of which is the updated graph.
(struct gfunc (f) #:property prop:procedure 0)

(define-syntax-rule (gλ args body0 body ...)
  (gfunc (λ args body0 body ...)))

(define-syntax (define/g stx)
  (syntax-case stx ()
    [(define/g (name g args ...) body0 body ...)
     (with-syntax* ([name (syntax-property #'name 'gfunc? #t #t)]
                    [name/g (format-id #'name "~a/g" #'name
                                      #:source #'name #:props #'name)])
       #'(begin
           (define name (gλ (g args ...) body0 body ...))
           (define (name/g args ...)
             (gλ (g) (name g args ...)))))]))

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

(begin-for-syntax
  (define (stx->props stx)
    (for/list ([k (syntax-property-symbol-keys stx)])
      (cons k (syntax-property stx k)))))

(define-syntax (see stx)
  (syntax-case stx ()
    [(see x) (with-syntax* ([xprops (datum->syntax stx
                                             #`(quote #,(stx->props stx))
                                             stx stx)])
         (define-values (s1 s2) (syntax-local-expand-expression #'x))
         (println (list s1 s2))
         #'xprops)]))

(see make-node)
(see #'make-node)
