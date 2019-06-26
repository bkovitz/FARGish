; yparse.rkt -- Experimental reader/parser/expander for y.brag

#lang debug at-exp racket

(require (for-syntax syntax/parse))
(require brag/support br-parser-tools/lex
         (only-in "y.brag" [parse bragparse])
         syntax/parse)

(define (tokenizer ip)
  (port-count-lines! ip)
  (define my-lexer
    (lexer-src-pos
      [(char-set ":;,.{}[]()=") lexeme] ; single-character tokens
      [(:or (char-set "+-*/<>") "==" "<=" "!=" ">=" "&&" "||")
       (token 'BINARY-OP lexeme)]
      ["--" lexeme]
      ["nodeclass" (token 'NODECLASS '(hash 'tag? #f))]
      ["tagclass" (token 'NODECLASS '(hash 'tag? #t))]
      ["codelet" 'CODELET]
      ["given" 'GIVEN]
      ["make" 'MAKE]
      ["in" 'IN]
      ["want" 'WANT]
      ["me" 'ME]
      ["STUB" 'STUB]
      [#\" (token 'STRING (list->string (get-string-token input-port)))]
      [(:: identifier-initial (:* identifier-subsequent))
       (token 'IDENTIFIER (string->symbol lexeme))]
      [whitespace (return-without-pos (my-lexer ip))]
      [(eof) 'EOF]))
  (λ () (my-lexer ip)))

(define get-string-token
  (lexer
   [(:~ #\" #\\) (cons (car (string->list lexeme))
                       (get-string-token input-port))]
   [(:: #\\ #\\) (cons #\\ (get-string-token input-port))]
   [(:: #\\ #\") (cons #\" (get-string-token input-port))]
   [#\" null]))

(define-lex-abbrevs
  [identifier-punctuation (char-set "!@$%^&*-_+=|<>?/")]
  [identifier-initial (:or alphabetic identifier-punctuation)]
  [identifier-subsequent (:or identifier-initial numeric)])

(define (->input-port src)
  (cond
    [(input-port? src) src]
    [(string? src) (open-input-string src)]
    [else (raise-arguments-error '->input-port
      "src cannot be converted to an input port; must be a string or already an input port"
      "src" src)]))

(define (tokenize src)
  (let ([th (tokenizer (->input-port src))])
    (let loop ([tok (th)])
      (displayln tok) ;DEBUG
      (when (not (eq? 'EOF (position-token-token tok)))
        (loop (th))))))

(define (parse src)
  (bragparse (tokenizer (->input-port src))))

;;;;;;;;;;;;;;

(define (compile-subgraph-defn stx)
  (syntax-parse stx
    [({~literal subgraph-defn}
       ({~literal node-chain}
          ({~literal node-ref}
            ({~literal nodeclass} class:id))) ...+)
     #'(λ (g)
         (cartesian-product (filter (node-is-a?/ g 'class) (all-nodes g))
                            ...))]))


     ;#'(λ (g) (filter (node-is-a?/ g 'class) (all-nodes g)))]))


;(let ([blah1s (filter (node-is-a?/ g 'blah1) (all-nodes g))]
;      [blah2s (filter (node-is-a?/ g 'blah2) (all-nodes g))])
;  (cartesian-product blah1s blah2s))
; Returns a single list of two-element lists


(define (compile-farg-spec stx)
  (syntax-parse stx
    [({~literal farg-spec} body:expr)
     (compile-subgraph-defn #'body)]))

(compile-farg-spec
  (parse "{ blah }"))

(compile-farg-spec
  #R (parse "{ blah1 blah2 }"))

