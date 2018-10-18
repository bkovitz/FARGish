; xtyped.rkt -- Experimenting with Typed Racket

#lang debug at-exp typed/racket

(struct is-a ([ancestors : (Setof Symbol)])
             #:prefab #:constructor-name make-is-a)

(define i (make-is-a (set 'a 'b 'c)))

(define (has? [i : is-a] [sym : Symbol]) : Boolean
  (set-member? (is-a-ancestors i) sym))

(define (check-many) : Void
  (for/list : (Listof (Listof Boolean))
    ([_ 1000000])
    (list (has? i 'a) (has? i 'not)))
  (void))

(time (check-many))

; cpu time: 6372 real time: 12174 gc time: 5484
; > (time (check-many))
; cpu time: 1512 real time: 1514 gc time: 780
; > (time (check-many))
; cpu time: 1528 real time: 1564 gc time: 772
; > (time (check-many))
; cpu time: 1576 real time: 1596 gc time: 840
; > (time (check-many))
; cpu time: 1528 real time: 1524 gc time: 784
; > (time (check-many))
; cpu time: 5520 real time: 6088 gc time: 4764
; > (time (check-many))
; cpu time: 1504 real time: 1520 gc time: 752
; > (time (check-many))
; cpu time: 1556 real time: 1557 gc time: 804
; > (time (check-many))
; cpu time: 1516 real time: 1526 gc time: 772

; racket -i -t xuntyped.rkt
;
; "xtyped.rkt"> (time (check-many))
; cpu time: 748 real time: 748 gc time: 268
; "xtyped.rkt"> (time (check-many))
; cpu time: 992 real time: 992 gc time: 532
; "xtyped.rkt"> (time (check-many))
; cpu time: 1208 real time: 1205 gc time: 720
; "xtyped.rkt"> (time (check-many))
; cpu time: 696 real time: 696 gc time: 216
; "xtyped.rkt"> (time (check-many))
; cpu time: 896 real time: 1397 gc time: 412
; "xtyped.rkt"> (time (check-many))
; cpu time: 1020 real time: 1021 gc time: 540
; "xtyped.rkt"> (time (check-many))
; cpu time: 1072 real time: 1074 gc time: 604
; "xtyped.rkt"> (time (check-many))
; cpu time: 1196 real time: 1194 gc time: 716
; "xtyped.rkt"> (time (check-many))
; cpu time: 768 real time: 769 gc time: 296
; "xtyped.rkt"> (time (check-many))
; cpu time: 788 real time: 786 gc time: 316
