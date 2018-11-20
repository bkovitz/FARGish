; Throwaway code to explore suspending and restarting

#lang debug at-exp racket

(require rackunit describe)

(provide maybe-suspend with-suspension-proc
         suspend? set-suspend! unset-suspend!)

(define suspend? #f)
(define resume-k #f)

(define (maybe-suspend v)
  (when suspend?
    (let/cc k
      (set! resume-k k)
      (suspend? v))))

;; 'resume' only returns if there is nothing suspended; then it returns void.
(define (resume)
  (let ([k resume-k])
    (if resume-k
      (begin
        (set! resume-k (void))
        (k))
      (abort-current-continuation (default-continuation-prompt-tag)
                                  (const (void))))))

(define (set-suspend! proc)
  (set! resume-k #f)
  (set! suspend? proc))

(define (unset-suspend!)
  (set! resume-k #f)
  (set! suspend? #f))

(define-syntax-rule (with-suspension-proc proc body ...)
  (begin
    (set-suspend! proc)
    (let ([result (begin body ...)])
      (unset-suspend!)
      result)))

(module+ test
  (test-case "maybe-suspend"
    (define (func n)
      (let loop ([i 0])
        (maybe-suspend i)
        (if (< i n)
          (loop (add1 i))
          i)))

    (define results '())

    (define (save-i i)
      (set! results (cons i results))
      (resume))

    (define wresult
      (with-suspension-proc save-i
        (set! results (cons (func 4) results))
        'w))

    (set! results (cons (func 6) results))

    (check-equal? (reverse results) '(0 1 2 3 4 4 6))
    (check-equal? wresult 'w)))

(module* example racket
  (require (submod ".."))

  ;; Here's how to do it in the REPL.

  ;; First, type this:  (require (submod "xsusp.rkt" example))

  (define (func n)
    (let loop ([i 0])
      (maybe-suspend i)
      (if (< i n)
        (loop (add1 i))
        i)))

  (with-suspension-proc (λ (i) (abort-current-continuation
                                 (default-continuation-prompt-tag) (const i)))
    (func 3))

  ; This will print "0". Now if you repeatedly type "(resume)" at the REPL
  ; prompt, you'll get the next value of i in func. After func has returned,
  ; you'll get (void).

  (eprintf "DONE")
  ;This doesn't quite work as expected: the abort-current-continuation
  ;removes the eprintf from the continuation.
)
