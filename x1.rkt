; Throwaway code

#lang debug at-exp racket

(require errortrace)
(require "wheel.rkt" "xsusp3.rkt" "model1.rkt" "shorthand.rkt"
         (prefix-in f: "fargish1.rkt")
         (only-in "fargish1.rkt"
           farg-model-spec nodeclass tagclass)
         (prefix-in g: "graph1.rkt")
         (only-in "graph1.rkt"
           pr-graph pr-group pr-node
           define/g gdo))
(require rackunit racket/pretty describe)

(define spec
  (farg-model-spec
    (nodeclass (letter a)
      (name a)
      (value a))
    (nodeclass (group nm)
      (name nm))))

(define group1
  (make-graph spec
    '(:in (group a-b)
       (:edge (letter a) out (letter b) in))))
