; observe.rkt -- Record and retrieve numerical observations

#lang debug at-exp racket

(require "wheel.rkt")
(require data/gvector racket/hash syntax/parse/define)
(require plot pict pict/convert)
(require rackunit racket/pretty describe profile)

(provide os os-prefix observing observe! tabl)

(define os-prefix (make-parameter '())) ; list of (key . value)
  ;TODO should guard that parameter is a list of key-value pairs?

(define os (make-gvector)) ; "observations": (key . value)s

(define-simple-macro (observing k v body:expr ...+)
  (parameterize ([os-prefix (append-item (os-prefix) (cons k v))])
    body ...))

(define observe!
  (case-lambda
    [(k v)
     (gvector-add! os (append-item (os-prefix) (cons k v)))
     v]
    [(ht)
     (for ([(k v) ht])
       (gvector-add! os (append-item (os-prefix) (cons k v))))
     ht]
    [(k1 k2 ht)
     (for ([(k v) ht])
       (gvector-add! os (append-item (os-prefix) (cons k1 k) (cons k2 v))))]
     ))

(define (arg-matches? arg o)
  (match arg
    [`(,k ,v)
      (cond
        [(assoc k o) => (λ (pair) (equal? v (cdr pair)))]
        [else #f])]
    [`(,k)
      (assoc k o)]))

(define (matches? args o)
  (let loop ([args args])
    (cond
      [(null? args) #t]
      [(arg-matches? (safe-take args 2) o)
       (matches? (safe-drop args 2) o)]
      [else #f])))

(define (top? arg)
  (match arg
    [`(TOP . ,_) #t]
    [else #f]))

(define (tabl . args)
  (let*-values ([(tops args) (partition top? args)])
    (cond
      [(null? tops)
       (for ([o os]
             #:when (matches? args o))
         (displayln o))]
      [else
        (match (car tops)
          [`(TOP ,n ,label-key ,numeric-key)
            (let* ([os* (filter (λ (o) (matches? args o)) (gvector->list os))]
                   [keys (set label-key numeric-key)]
                   [ignore-key? (λ (kv) (set-member? keys (car kv)))]
                   [groups (group-by (λ (o)
                                       (remf* ignore-key? o))
                                     os*)]
                   [groups (for/list ([group groups])
                             (safe-take
                               (sort group >
                                     #:key (λ (o) (cdr (assoc numeric-key o))))
                               n))])
              (pretty-print groups))])])))



;(for ([t 10])
;  (observing 't t
;    (for ([node (list 'a 'b 'c)])
;      (observing 'node node
;        (observe! 'ac (random))))))
;
;(tabl 'node 'c)

;(tabl '(TOP 20 node ac))
