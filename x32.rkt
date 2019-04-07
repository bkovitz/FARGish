; x32.rkt -- Experimenting with building up a graph to represent the temporal
;            trace for the solved "32" problem

#lang debug at-exp typed/racket
(require typed/debug/report)

(require errortrace)
(require typed/json)
(require "typed-wheel.rkt")
(require "types.rkt" "fargish.rkt" "model.rkt" "trace.rkt")

(: ->num-digits : (U Integer Void) -> (U Integer Void))
(define (->num-digits x)
  (safe-string-length (safe-integer->string x)))

(define-spec spec
  ; Nodes
  (nodeclass problem)
  (nodeclass trace)
  (nodeclass (t [t : Integer])  ; a timestep in a trace
    (value t)
    (display-name (format "t~a" t)))
  (nodeclass equation)
  (nodeclass (number [n : Integer])
    (value n)
    (display-name n))
  ;(nodeclass placeholder)
  (nodeclass times
    (display-name "*"))
  (nodeclass equals
    (display-name "="))
  ; Tags
  (tagclass target
    (applies-to ([node])))
  (tagclass brick
    (applies-to ([node])))
;  (tagclass greater-than
;    (applies-to ([greater (of-class 'number)
;                          (by-ports 'greater 'greater-than)]
;                 [lesser (of-class 'number) (by-ports 'lesser 'less-than)])
;      (condition (value-pred?/g safe->? greater lesser))))
;  (tagclass same
;    (applies-to ([node1 as-tag] [node2 as-tag]) ;TODO not same node
;      (condition (and?/g (same-class?/g node1 node2)
;                         (value-pred?/g equal? node1 node2)))))
  (tagclass (num-digits [n : Integer])
    (value n)
    (display-name (format "num-digits-~a" n))
    (applies-to ([node (of-class number)])
      ;(condition (= n (->num-digits node))) ; IDEAL
      (condition (safe-eqv? (cast (value-of g this) (U Integer Void))
                            (->num-digits (cast (value-of g node) (U Integer Void)))))))
;      (condition (value-pred?/g
;                   (λ (v) (safe-eqv? n (->num-digits v)))
;                   node))))
  (tagclass fill
    (applies-to ([node-to-fill (by-ports fill filled-by)]
                 ;TODO Make sure node-to-fill can be filled
                 [filler-node (by-ports filled-by fill)])))
  (tagclass can-be-consumed
    (applies-to ([node])))
  (tagclass consume
    (applies-to ([consumer (by-ports consumer filled-by)]
                 [consumes (by-ports consumes fill)])))

  ;(chain equation prev -> next ...)
  )


;; ======================================================================
;;
;; The solution to the "32" problem, as a stored temporal trace
;;

(define g
  (let
    ([(g) (make-empty-graph spec)]
     ;old temporal trace
     [(g trace) (make-node g (trace))]
     ;t0
     [(g t0) (make-node/in g trace (t 0))]
     [(g) (add-edge g `((,trace first) (,t0 first-in)))]
     ; problem
     [(g problem) (make-node/in g t0 'problem)]
     [(g target) (make-node/in g problem (number 32))]
     [(g) (add-tag g 'target target)]
     [(g b6) (make-node/in g problem (number 6))]
     [(g) (add-tag g 'brick b6)]
     [(g b3) (make-node/in g 'problem (number 3))]
     [(g) (add-tag g 'brick b3)]
     [(g b4) (make-node/in g 'problem (number 4))]
     [(g) (add-tag g 'brick b4)]
     [(g b5) (make-node/in g 'problem (number 5))]
     [(g) (add-tag g 'brick b5)]

     ;t1
     [(g t1) (make-node/in g trace (t 1))]
     [(g) (add-edge g `((,t0 next) (,t1 prev)))]
     [(g eqn1) (make-node/in g t1 'equation)]
     [(g e6) (make-node/in g eqn1 (number 6))]
     [(g) (add-edge g `((,eqn1 first) (,e6 first-in)))]
     [(g c1) (make-tag g 'consume (list e6 b6))]
     [(g times) (make-node/in g eqn1 'times)]
     [(g) (add-edge g `((,e6 next) (,times prev)))]
     [(g e4) (make-node/in g eqn1 (number 4))]
     [(g c2) (make-tag g 'consume (list e4 b4))]
     [(g) (add-edge g `((,times next) (,e4 prev)))]
     [(g equals1) (make-node/in g eqn1 'equals)]
     [(g) (add-edge g `((,e4 next) (,equals1 prev)))]
     [(g e24) (make-node/in g eqn1 (number 24))]
     [(g) (add-edge g `((,equals1 next) (,e24 prev)))]

;     [(g) (make-permanent g trace
;                          t0 problem target b6 b3 b4 b5 c1 c2
;                          t1 eqn1 e6 times e4 equals1 e24)]
     )
    g))

(pr-graph g)

(write-json (graph->jsexpr g))
