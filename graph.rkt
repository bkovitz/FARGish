#lang racket

;; Data structure for port graphs

(require rackunit data/collection racket/generic racket/struct "id-set.rkt")

(provide has-node?)

;; A port graph
(struct graph (elems id-set spec) #:transparent)

(define empty-spec '())
(define empty-graph (graph #hash() empty-id-set empty-spec))

(define (has-node? g id)
  (hash-has-key? (graph-elems g) id))

(module+ test
  (check-false (has-node? empty-graph 'plus)))

;; A hack for now. This should fill in the attrs with defaults from the
;; class definition in the spec. If attrs is just a symbol or number,
;; we should find an appropriate class definition. For now, though, we
;; just hard-code a couple things.
(define (normalize-attrs attrs)
  (match attrs
    [(? symbol? letter)
     (make-immutable-hash `((name . ,letter) (class . letter)))]
    [(hash-table ('name _) _ ...)
     attrs]
    [(hash-table ('class cl) _ ...)
     (hash-set attrs 'name cl)]
    [(? list?)
     (normalize-attrs (make-immutable-hash attrs))]
    [_ (raise-arguments-error 'normalize-attrs "invalid node attributes"
                              "attrs" attrs)]))

(module+ test
  (check-equal? (normalize-attrs 'a)
                #hash((class . letter) (name . a)))
  (check-equal? (normalize-attrs '((class . plus)))
                #hash((class . plus) (name . plus)))
  (check-equal? (normalize-attrs '((class . plus) (name . xyz)))
                #hash((class . plus) (name . xyz))))

;(define (make-node g attrs) ;returns g* id  (two values)
;  (let*-values ([(g id) (make-id g attrs)]
;                [(g) (struct-copy graph g
;                       [elems (hash-set (graph-elems g) id
;                                        (
