; bug.rkt -- Minimal runnable example for a possible bug found in 'brag'
;
; Found by Ben Kovitz bkovitz@indiana.edu on 28-Apr-2019.

#lang debug racket

(require brag/support br-parser-tools/lex
         (only-in "bug.brag" parse))

(define (tokenize ip)
  (port-count-lines! ip)
  (define my-lexer
    (lexer-src-pos
      [(char-set "(),:") lexeme]  ; single-character tokens
      ["thing" 'THING]
      [(:+ alphabetic) (token 'IDENTIFIER (string->symbol lexeme))]
      [whitespace (return-without-pos (my-lexer ip))]))
  (λ () #R (my-lexer ip)))

(define (p str)
  (parse (tokenize (open-input-string str))))

(define t0 "thing(a)")                      ; This works.
(define t1 "thing(a) thing(b)")             ; This works.
(define t2 "thing(a : Integer)")            ; This works.
(define t3 "thing(a) thing(b : Integer)")   ; This works.
(define t4 "thing(a : Integer) thing(b)")   ; This doesn't work. The parser
                                            ; says that the second "thing" is
                                            ; an error.
(p t0)
(p t1)
(p t2)
(p t3)
(p t4)
