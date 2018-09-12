#lang racket

;; id-set: a data structure to assign unique ids to named objects
;;
;; Each object gets an id assigned by appending a "suffix" to a given "base".
;; Each unique "base" gets its own series of suffixes.

(require rackunit)

(provide id-set empty-id-set gen-id remove-id)

(struct id-set (base->suffix all-ids) #:transparent)

(define empty-id-set (id-set #hash() (set)))

(define (gen-id an-id-set base) ;returns an-id-set* id
  (match-define (id-set base->suffix all-ids) an-id-set)
  (define (loop base->suffix id)
    (if (set-member? all-ids id)
        (call-with-values (λ () (bump-base base->suffix base)) loop)
        (values (id-set base->suffix (set-add all-ids id)) id)))
  (call-with-values (λ () (if (hash-has-key? base->suffix base)
                              (bump-base base->suffix base)
                              (init-base base->suffix base)))
                    loop))

(define (remove-id an-id-set id)
  (match-define (id-set h all-ids) an-id-set)
  (id-set h (set-remove all-ids id)))

(define (string-last s)
  (if (non-empty-string? s)
      (string-ref s (sub1 (string-length s)))
      (void)))

(define (char-add1 c)
  (integer->char (add1 (char->integer c))))

(define (bump-string s)
  (list->string
    (let loop ([ns-done '()] [ns-to-go (reverse (string->list s))])
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

;; Returns initial suffix, next suffix
(define (init-suffix base)
  (if (char-numeric? (string-last (~a base)))
      (values "" "a")
      (values "" 2)))

(define (init-base id-hash base)
  (define-values (suffix next-suffix) (init-suffix base))
  (define id-hash* (hash-set id-hash base next-suffix))
  (values id-hash* (string->symbol (~a base suffix))))

(define (bump-base id-hash base)
  (define suffix (hash-ref id-hash base))
  (values (hash-update id-hash base (if (number? suffix)
                                        add1
                                        bump-string))
          (string->symbol (~a base suffix))))

(module+ test
  (let*-values ([(h plus) (gen-id empty-id-set 'plus)]
                [(h target15) (gen-id h 'target15)]
                [(h plus2) (gen-id h 'plus)]
                [(h target15a) (gen-id h 'target15)])
    (check-equal? plus 'plus)
    (check-equal? target15 'target15)
    (check-equal? plus2 'plus2)
    (check-equal? target15a 'target15a)
    (check-equal? (id-set-all-ids h) (set 'plus 'target15 'plus2 'target15a))))

