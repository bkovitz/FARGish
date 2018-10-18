; xuntyped.rkt -- Experimenting: comparing performance with Typed Racket

#lang debug at-exp racket

(struct is-a (ancestors)
             #:prefab #:constructor-name make-is-a)

(define i (make-is-a (set 'a 'b 'c)))

(define (has? i sym)
  (set-member? (is-a-ancestors i) sym))

(define (check-many)
  (for/list
    ([_ 1000000])
    (list (has? i 'a) (has? i 'not)))
  (void))

(time (check-many))

; DrRacket:
;
; cpu time: 5956 real time: 9117 gc time: 5044
; > (time (check-many))
; cpu time: 1460 real time: 1488 gc time: 648
; > (time (check-many))
; cpu time: 1456 real time: 1518 gc time: 644
; > (time (check-many))
; cpu time: 1568 real time: 2042 gc time: 764
; > (time (check-many))
; cpu time: 1448 real time: 1451 gc time: 644
; > (time (check-many))
; cpu time: 1476 real time: 1477 gc time: 656
; > (time (check-many))
; cpu time: 3440 real time: 3439 gc time: 2616
; > (time (check-many))
; cpu time: 1332 real time: 1330 gc time: 532

; racket -i -t xuntyped.rkt
;
; "xuntyped.rkt"> (time (check-many))
; cpu time: 1044 real time: 1430 gc time: 500
; "xuntyped.rkt"> (time (check-many))
; cpu time: 780 real time: 828 gc time: 232
; "xuntyped.rkt"> (time (check-many))
; cpu time: 1208 real time: 1410 gc time: 664
; "xuntyped.rkt"> (time (check-many))
; cpu time: 872 real time: 873 gc time: 328
; "xuntyped.rkt"> (time (check-many))
; cpu time: 1012 real time: 1013 gc time: 456
; "xuntyped.rkt"> (time (check-many))
; cpu time: 1044 real time: 1045 gc time: 508
; "xuntyped.rkt"> (time (check-many))
; cpu time: 756 real time: 755 gc time: 212
; "xuntyped.rkt"> (time (check-many))
; cpu time: 1036 real time: 1038 gc time: 512
; "xuntyped.rkt"> (time (check-many))
; cpu time: 1092 real time: 1093 gc time: 540
