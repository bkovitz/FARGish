; x.ss -- Experiments in defining and evaluating painters in Scheme

(load "pmatch.ss")

;; Version 1

(define (run-painter env painter)
  (pmatch painter
    [(,source ,target ,func)
     (paint (get-det-addr target)
            (apply-func func (get-value (get-det-addr source))))]))

;       ([source-addr
;         (paint target-addr (apply-func func (get-source-value source)))]))

(define (paint det-addr value)
  (display (list 'paint det-addr value)))

;; Version 2

(define (run-painter env painter)
  (pmatch painter
    [(,source ,target ,func)
      (let* ([env (setvar env 'I (eval-as env 'det-addr source))]
             [env (setvar env 'J (eval-as env 'det-addr target))]
             [env (setvar env 'F (eval-as env 'func func))]
             [env (setvar env 'V (apply-func
                                   (eval-as env 'func 'F)
                                   (eval-as env 'value-at 'I)))])
         (paint env 'J 'V))]))

(define (paint env target value)
  (display (list (eval-as env 'det-addr target)
                 (eval-as env 'value-at value))))

;; Version 3

(define (run-painter env painter)
  (pmatch painter
    [(,source ,target ,func)
      (let* ([I (eval-as env 'det-addr source)]
             [J (eval-as env 'det-addr target)] ; no good: target may refer to I
             [F (eval-as env 'func func)]
             [V (apply-func env F (eval-as env 'value-at I))])
        (paint env J V))]))

(define (paint env det-addr value)
  (display (list 'paint det-addr value)))


;; Version 4

(define (run-painter env painter)
  (pmatch painter
    [(,source ,target ,func)
      (let*-values ([(env I) (setvar env 'I (eval-as env 'det-addr source))]
                    [(env J) (setvar env 'J (eval-as env 'det-addr target))]
                    [(env F) (setvar env 'F (eval-as env 'func func))]
                    [(V) (apply-func env F (eval-as env 'value-at I))])
        (paint env J V))]))

  ; Are setvar and paint the same thing?


(define (same v)
  v)

(define (succ v)
  (integer->char (add1 (char->integer v))))

(define (get-det-addr addr)
  addr)

(define (get-value det-addr)
  (string-ref canvas (- det-addr 1)))

(define (apply-func func value)
  (func value))

(define abs-painter `(1 3 ,succ))

(define qpainter `((x (+ x 2) ,same)
                   working-soup
                   (x (+ x 1) j)))

(define canvas "a     ")

(run-painter '() abs-painter)
