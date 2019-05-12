; fb2.rkt -- Experimental reader/parser/expander for fb.brag
;
; This file should probably become the reader and expander for FARGish.

#lang debug at-exp racket

(require (for-syntax racket syntax/parse)
         brag/support br-parser-tools/lex
         (only-in "fb.brag" parse)
         syntax/parse)

(define (tokenize ip)
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

;; ======================================================================
;;
;; Making something out of the syntax objects from the parser
;;

; name : Symbol
; default-attrs : (Hashof Symbol Any)
; args : (Listof Arg)
; ancestors : (Listof Symbol)
; body :
(struct Nodeclass (name default-attrs args ancestors body)
                  #:prefab)
(struct Arg (name type)
            #:prefab)
(struct Nodeclass-Body-Elem (lhs rhs)
        #:prefab)

;(define (f fargish-spec-stx)
;  (syntax-parse fargish-spec-stx
;    [(_ elem ...)
;     #'(list elem ...)]))

(define (f fargish-spec-stx)
  fargish-spec-stx)
  ;#`(list #,@fargish-spec-stx))

(define-syntax (farg-spec stx)
  (syntax-parse stx
    [(_ elem ...)
     #'(list elem ...)]))

(define-syntax (nodeclass-definition nodeclass-stx)
  (syntax-parse nodeclass-stx
    [(_ ht name args ancestors body)
     #'(Nodeclass 'name ht args ancestors 'body)]))

(define-syntax (args stx)
  (syntax-parse stx
    [(_) #''()]
    [(_ (arg ...))
     #'(list arg ...)]))

(define-syntax (untyped-arg stx)
  (syntax-parse stx
    [(_ id:id) #'(Arg 'id 'Any)]))

(define-syntax (typed-arg stx)
  (syntax-parse stx
    [(_ id:id typename:expr)
     #'(Arg 'id 'typename)]))

(define-syntax (ancestors stx)
  (syntax-parse stx
    [(_ id:id ...) #'(list 'id ...)]))

(define-syntax (nodeclass-body stx)
  (syntax-parse stx
    [(_ elem:expr ...) #'(list elem ...)]))

(define-syntax (nodeclass-body-elem stx)
  (syntax-parse stx
    [(_ lhs:expr rhs:expr)
     #'(Nodeclass-Body-Elem 'lhs 'rhs)]))

(struct Codelet (name match-elems make-elems) #:prefab)

(struct MatchElemInfo (name node-condition) #:prefab)

(struct MatchElemsInfo (elem-infos collective-condition) #:prefab)

(define (reverse-MatchElemsInfo info)
  (match-let ([(MatchElemsInfo elem-infos collective-condition) info])
    (MatchElemsInfo (reverse elem-infos) (reverse collective-condition))))

(define (add-name-and-condition s name condition)
  (let ([already-defined? (λ (info)
                            (equal? name (MatchElemInfo-name info)))]
        [existing-infos (MatchElemsInfo-elem-infos s)])
    (cond
      [(ormap already-defined? existing-infos)
       (raise @~a{Already defined: @name})] ;TODO Better exception
      [else (struct-copy MatchElemsInfo s
              [elem-infos (cons (MatchElemInfo name condition)
                                existing-infos)])])))

(define (add-collective-condition s elem)
  (let ([existing (MatchElemsInfo-collective-condition s)])
    (struct-copy MatchElemsInfo s
      [collective-condition (cons elem existing)])))

(define (match-elems->info elems)
  (for/fold ([s (MatchElemsInfo '() '())]
             #:result (reverse-MatchElemsInfo s))
            ([elem elems])
    (match elem
      [(NamedNode name condition)
       (add-name-and-condition s name condition)]
      [(Expr body)
       (add-collective-condition s elem)]))) ;STUB

(struct NamedNode (name condition) #:prefab)

(struct NodeRef (body) #:prefab)

(struct Expr (body) #:prefab)

(struct ChainExpr (body) #:prefab)

(struct ChainLink (body) #:prefab)

(define-syntax (codelet stx)
  (syntax-parse stx
    [(_ name:id ((~literal match-elems) match-elem:expr ...)
                ((~literal make-elems) make-elem:expr ...))
     #'(Codelet 'name (list match-elem ...) (list make-elem ...))]))

(define-syntax (named-node stx)
  (syntax-parse stx
    [(_ name:id condition:expr)
     #'(NamedNode 'name condition)]))

(define-syntax (node-ref stx)
  (syntax-parse stx
    [(_ body:expr ...)
     #'(NodeRef (list body ...))]))

(define-syntax (node-class stx)
  (syntax-parse stx
    [(_ name:id)
     #''name]))

(define-syntax (expr stx)
  (syntax-parse stx
    [(_ body:expr ...)
     #'(Expr (list body ...))]))

(define-syntax (chain-expr stx)
  (syntax-parse stx
    [(_ body:expr ...)
     #'(ChainExpr (list body ...))]))

(define-syntax (chain-link stx)
  (syntax-parse stx
    [(_ body:expr ...)
     #'(ChainLink (list body ...))]))

(define-syntax (already-named-node stx)
  (syntax-parse stx
    [(_ name:id)
     #''name]))

;------ Throwaway test code

(define (lx str)
  (let ([th (tokenize (open-input-string str))])
    (let loop ([tok (th)])
      (displayln tok)
      (when (not (eq? 'EOF (position-token-token tok)))
        (loop (th))))))

(define (p str)
  (parse (tokenize (open-input-string str))))

(define t0 "in a;")
(define t1 "in a; in b;")
(define t2 "in a; in { b }")
(define t3 "in a(x);")
(define t4 "in a(x); in b;")
(define t5 "in a(x : Integer); in b;")

(define s0 "nodeclass a;")

(define s1 "nodeclass a; nodeclass b;")

(define s2 "nodeclass number(n : Integer);")

(define s3 "
nodeclass number(n : Integer);

nodeclass blah;
")

(define s4 "nodeclass a : b;")
(define s5 "nodeclass a() : b;")
(define s6 "nodeclass a : b; nodeclass c;")
(define s7 "nodeclass a() : b; nodeclass c;")
(define s8 "nodeclass a(n : Integer) : b; nodeclass c;")
(define s9 "nodeclass a(n : Integer) : b { blah = yah; } nodeclass c;")
(define s10 "nodeclass a(x) : b, c { value = x; }")


(define u0 "codelet gtc
given {
  [n1 : number] [n2 : number]
  { n1.value > n2.value }
} make {
  n1.gt.n2
}")

(define-namespace-anchor ns-anchor)
(define ns (namespace-anchor->namespace ns-anchor))

(define a (f (p s4)))
(displayln a)
(define b (eval a ns))
b
(newline)
(define aa (f (p s8)))
(displayln aa)
;(define bb (eval aa))
;bb
(newline)
(define a1 (f (p s10)))
(displayln a1)
;(define b1 (eval a1))
;b1
(newline)
(define a2 (f (p u0)))
;a2
(pretty-print (syntax->datum a2))
(define b2 (eval a2 ns))
b2
(define c (car b2))
(define ms (Codelet-match-elems c))
(match-elems->info ms)

