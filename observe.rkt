; observe.rkt -- Record and retrieve numerical observations

#lang debug at-exp racket

(require "wheel.rkt")
(require data/gvector racket/hash syntax/parse/define)
(require plot pict pict/convert)
(require rackunit racket/pretty describe profile)

;(provide os os-n os-prefix observing observe! see)

(define os-prefix (make-parameter '())) ; list of (key . value)
  ;TODO should guard that parameter is a list of key-value pairs?

(define os (make-gvector)) ; "observations": (key . value)s

(define-simple-macro (observing k v body:expr ...+)
  (parameterize ([os-prefix (append-item (os-prefix) (cons k v))])
    body ...))

(define (observe! k v)
  (gvector-add! os (append-item (os-prefix) (cons k v))))

(define (arg-matches? arg o)
  (match-define `(,k ,v) arg)
  (cond
    [(assoc k o) => (λ (pair) (equal? v (cdr pair)))]
    [else #f]))

(define (matches? args o)
  (let loop ([args args])
    (cond
      [(null? args) #t]
      [(arg-matches? (take args 2) o)
       (matches? (drop args 2) o)]
      [else #f])))

(define (table . args)
  (for ([o os]
        #:when (matches? args o))
    (displayln o)))

(for ([t 10])
  (observing 't t
    (for ([node (list 'a 'b 'c)])
      (observing 'node node
        (observe! 'ac (random))))))

(table 'node 'c)
