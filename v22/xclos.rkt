; Experiments with closures

#lang debug at-exp racket

(require syntax/parse syntax/parse/define
         (for-syntax syntax/parse))
(require racket/hash)
;(require predicates)
(require "wheel.rkt")
(require expect/rackunit (only-in rackunit test-case))

;(require srfi/1)


(define (chain-funcs combine stop? init-value . funcs)
  (λ args
    (let loop ([funcs funcs] [result-so-far init-value])
      (cond
        [(null? funcs) result-so-far]
        [else
          (let* ([f (car funcs)]
                 [f-result (apply f args)])
            (cond
              [(stop? f-result) f-result]
              [else (loop (cdr funcs) (combine result-so-far f-result))]))]))))



;(chain-funcs safe-*
;             (eq?? 'reject)
;             (void)
;             (map desideratum->node->acceptability desiderata))
