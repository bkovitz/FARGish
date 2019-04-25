#lang racket

(require br-parser-tools/lex
         br-parser-tools/yacc
         (prefix-in : br-parser-tools/lex-sre))

(define-tokens value-tokens (IDENTIFIER STRING NODECLASS))
(define-empty-tokens keywords
  (CODELET GIVEN MAKE EOF LEFT-BRACE RIGHT-BRACE LEFT-BRACKET RIGHT-BRACKET
   LEFT-PAREN RIGHT-PAREN COMMA COLON SEMICOLON DOUBLE-HYPHEN DOT))

(define (tokenize ip)
  (port-count-lines! ip)
  (define my-lexer
    (lexer-src-pos
      [#\{ 'LEFT-BRACE]
      [#\} 'RIGHT-BRACE]
      [#\[ 'LEFT-BRACKET]
      [#\] 'RIGHT-BRACKET]
      [#\( 'LEFT-PAREN]
      [#\) 'RIGHT-PAREN]
      [#\, 'COMMA]
      [#\: 'COLON]
      [#\; 'SEMICOLON]
      ["--" 'DOUBLE-HYPHEN]
      [#\" (token-STRING (list->string (get-string-token input-port)))]
      ["nodeclass" (token-NODECLASS (hash))]
      ["tagclass" (token-NODECLASS (hash 'tag? #t))]
      ["codelet" 'CODELET]
      ["given" 'GIVEN]
      ["make" (token-MAKE)]
      [(:: identifier-initial (:* identifier-subsequent))
       (token-IDENTIFIER (string->symbol lexeme))]
      [#\. 'DOT]
      [whitespace (return-without-pos (my-lexer ip))]
      [(eof) 'EOF]))
  (thunk (my-lexer ip)))

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

(define (lx str)
  (let ([th (tokenize (open-input-string str))])
    (let loop ([tok (th)])
      (displayln tok)
      (when (not (eq? 'EOF (srcloc-token-token tok)))
        (loop (th))))))

(define parse
  (parser 
    (src-pos)
    (start start)
    (end EOF)
    (tokens keywords value-tokens)
    (error (λ args (displayln args)))
    (grammar
      (start [(fargish-elems) 'stub])
      (fargish-elems
        [() '()]
        [(fargish-elems fargish-elem) (cons $2 $1)])
      (fargish-elem
        [(nodeclass-definition) 'stub]
        [(codelet) 'stub])
      (codelet
        [(CODELET codelet-name given make)
         'stub])
      (codelet-name
        [(IDENTIFIER) $1])
      (given
        [(GIVEN LEFT-BRACE given-body RIGHT-BRACE)
         'stub])
      (given-body
        [(given-elem) 'stub]
        [(given-body given-elem) 'stub])
      (given-elem
        [(chain) 'stub]
        [(boolean-expr SEMICOLON) 'stub])
      (make
        [(MAKE LEFT-BRACE make-body RIGHT-BRACE)
         'stub])
      (make-body
        [(chain) 'stub]   ; Do we need a building-chain to allow args to nodes?
        [(make-body chain) 'stub])
      (boolean-expr
        [() 'stub])
      (nodeclass-definition
        [(NODECLASS IDENTIFIER SEMICOLON)
         'stub]
        [(NODECLASS IDENTIFIER LEFT-BRACE nodeclass-body RIGHT-BRACE)
         'stub])
      (explicit-edge
        [(LEFT-PAREN port-label DOUBLE-HYPHEN port-label RIGHT-PAREN)
         'stub])
      (nodeclass-body
        [()
         'stub])
      (named-node
        [(LEFT-BRACKET node-name COLON nodeclass RIGHT-BRACKET)
         'stub])
      (chain
        [(nodespec) 'stub]
        [(chain DOT nodespec) 'stub])
      (nodespec
        [(IDENTIFIER) 'stub]
        [(named-node) $1])
      (port-label
        [(IDENTIFIER) $1])
      (node-name
        [(IDENTIFIER) $1])
      (nodeclass
        [(IDENTIFIER) $1])
      )))

(define (p str)
  (parse (tokenize (open-input-string str))))

