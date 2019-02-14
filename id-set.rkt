; id-set.rkt -- a data structure to assign unique ids to named objects
;
; Each object gets an id assigned by appending a "suffix" to a given "base".
; Each unique "base" gets its own series of suffixes.

#lang debug at-exp typed/racket

(module+ test
  (require typed/rackunit))

(provide empty-id-set gen-id remove-id all-ids
         Id IdSet)

(define-type Id (U Symbol Integer))
(define-type Base (U Symbol String Integer))
(define-type Suffix (U String Integer))

(define-type (Hashof K V) (Immutable-HashTable K V))

(struct IdSet ([ht/base->suffix : (Hashof Base Suffix)]
               [all-ids : (Setof Id)])
              #:prefab)

(define empty-id-set : IdSet (IdSet #hash() (set)))

(: gen-id (IdSet Base -> (Values IdSet Id)))
(define (gen-id id-set base)
  (match-let ([(IdSet ht all-ids) id-set])
    (letrec ([loop : ((Hashof Base Suffix) Id -> (Values IdSet Id))
               (λ (ht id)
                  (if (set-member? all-ids id)
                    (bump-base ht base loop)
                    (values (IdSet ht (set-add all-ids id))
                            id)))])
      (if (hash-has-key? ht base)
        (bump-base ht base loop)
        (init-base ht base loop)))))

(: remove-id (IdSet Id -> IdSet))
(define (remove-id id-set id)
  (match-define (IdSet h all-ids) id-set)
  (IdSet h (set-remove all-ids id)))

(: all-ids (IdSet -> (Setof Id)))
(define all-ids IdSet-all-ids)

(module+ test
  (test-case "id-set"
    (let*-values ([(h plus) (gen-id empty-id-set 'plus)]
                  [(h target15) (gen-id h 'target15)]
                  [(h plus2) (gen-id h 'plus)]
                  [(h target15a) (gen-id h 'target15)])
      (check-equal? plus 'plus)
      (check-equal? target15 'target15)
      (check-equal? plus2 'plus2)
      (check-equal? target15a 'target15a)
      (check-equal? (all-ids h) (set 'plus 'target15 'plus2 'target15a)))
    (let-values ([(h four) (gen-id empty-id-set 4)])
      (check-equal? four 4))))

;; ======================================================================
;;
;; Internal functions
;;

;(define-type (K a ...) ((Hashof Base Suffix) Id -> a))
;
;(define-syntax-rule (With-K (arg ...) k-arg ...)
;  (All (a ... ...) 

; Calls k with hash table updated for new base and the first id from that base.
(: init-base (All (a ...) ((Hashof Base Suffix) Base ((Hashof Base Suffix) Id -> (Values a ...)) -> (Values a ...))))
(define (init-base ht/base->suffix base k)
  (let*-values ([(suffix next-suffix) (init-suffix base)]
                [(ht/base->suffix) (hash-set ht/base->suffix base next-suffix)]
                [(id) (if (and (not (non-empty-string? suffix))
                               (integer? base))
                        base
                        (->symbol base suffix))])
    (k ht/base->suffix id)))

; Generates new suffix for base. Calls k with hash table updated for new suffix
; and the new id containing base and that suffix.
(: bump-base (All (a ...) ((Hashof Base Suffix) Base ((Hashof Base Suffix) Id -> (Values a ...)) -> (Values a ...))))
(define (bump-base ht/base->suffix base k)
  (let* ([suffix (hash-ref ht/base->suffix base)]
         [next-suffix (if (integer? suffix)
                        (add1 suffix)
                        (bump-string suffix))]
         [ht/base->suffix (hash-set ht/base->suffix base next-suffix)]
         [id (->symbol base suffix)])
    (k ht/base->suffix id)))

; Returns initial suffix, next suffix
(: init-suffix (Base -> (Values Suffix Suffix)))
(define (init-suffix base)
  (if (last-char-numeric? base)
    (values "" "a")
    (values "" 2)))

(: last-char-numeric? (Base -> Boolean))
(define (last-char-numeric? base)
  (let ([c (string-last (->string base))])
    (and (char? c) (char-numeric? c))))
  
(: bump-string (String -> String))
(define (bump-string s)
  (list->string
    (let loop ([ns-done : (Listof Char) '()]
               [ns-to-go (reverse (string->list s))])
      (match ns-to-go
        ['() (cons #\a ns-done)]
        [(list #\z more ...)
         (loop (cons #\a ns-done) more)]
        [(list letter more ...)
         `(,@(reverse more) ,(char-add1 letter) ,@ns-done)]))))


(module+ test
  (check-equal? (bump-string "") "a")
  (check-equal? (bump-string "a") "b")
  (check-equal? (bump-string "z") "aa")
  (check-equal? (bump-string "az") "ba")
  (check-equal? (bump-string "zz") "aaa")
  (check-equal? (bump-string "zgz") "zha")
  (check-equal? (bump-string "zgy") "zgz")
  (check-equal? (bump-string "bzz") "caa")
  (check-equal? (bump-string "zzz") "aaaa"))

;; This is faster than ~a
(: ->string (Any -> String))
(define (->string x)
  (cond
    [(string? x) x]
    [(symbol? x) (symbol->string x)]
    [(number? x) (number->string x)]
    [else (~a x)]))

(: ->symbol (Base Suffix -> Symbol))
(define (->symbol base suffix)
  (string->symbol (string-append (->string base)
                                 (->string suffix))))

(: char-add1 (Char -> Char))
(define (char-add1 c)
  (integer->char (add1 (char->integer c))))

(: string-last (String -> (U Char Void)))
(define (string-last s)
  (if (non-empty-string? s)
      (string-ref s (sub1 (string-length s)))
      (void)))
