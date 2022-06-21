; x.ss -- Experiments in defining and evaluating painters in Scheme

(load "pmatch.ss")

(define (run-painter env painter)
  (pmatch painter
    [(,source ,target ,func)
     (let*
       ([source-addr
         (paint target-addr (apply-func func (get-source-value source)))]))


(define (paint addr value)
  (display (list 'paint addr value)))

(define (same v)
  v)

(define qpainter `((x (+ x 2) ,same)
                   working-soup
                   (x (+ x 1) j)))

(run-painter '() qpainter)

yo yo yo
