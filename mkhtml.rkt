; mkhtml.rkt -- Makes the fargish.html file

#lang racket

(require txexpr data/collection data/pvector (except-in sugar repeat))

(define left-to-right
  (case-lambda
    [(ls)
       (for/fold ([pv (pvector)]
                  [x 0]
                  #:result (sequence->list pv))
                 ([elem ls])
         (values (conj pv (attr-set elem 'x x))
                 (+ x (->int (attr-ref elem 'width 10)))))]
    [(count item)
     (left-to-right (make-list count item))]))

(define temporal-trace
  `((rect ((class "trace") (width "600") (height "25") (rx "3") (ry "3")))
    ,@(left-to-right 10 `(rect ((class "tstep") (width "20") (height "20"))))))

(define html
  `(html
     (head (title "FARG model")
           (link ((rel "stylesheet") (type "text/css") (href "fargish.css")))
           #;(link ((rel "stylesheet") (type "text/css") (href "force-interactive.css")))
           )
     (body
       (p "t=" (span ((id "t")) "0"))
       (svg ((id "ws") (width "960") (height "600"))
         ;,@temporal-trace
         )
       (p
         (button ((id "step") (onClick "step_button()")) "t+1")
         (button ((is "reset") (onClick "reset_button()")) "reset"))
       (script ((src "jquery.js")))
       ;(script ((src "//d3js.org/d3.v3.min.js")))
       (script ((src "https://d3js.org/d3.v5.min.js")))
       (script ((src "force-directed3.js")))
       ;(script ((src "farg-model.js")))
       )))

(with-output-to-file "fargish.html" #:mode 'text #:exists 'replace
  (λ ()
    (displayln "<!DOCTYPE HTML plus SVG>")
    (displayln (xexpr->html html))))
