; fargish.rkt  --  FARGish as a #lang? Implemented in Typed Racket?

#lang typed/racket

(require syntax/parse syntax/parse/define
         (for-syntax syntax/parse))
(require "wheel.rkt")

; Generate make-node in the main macro?

(struct raw-nodeclass* ([name : Symbol]
                        [args : (Listof Symbol)]
                        [display-name : (-> 
