#lang racket

(require "data.rkt")
(require "../ast.rkt")
(require "grammar.rkt")
(require "../pretty.rkt")
(require (only-in "../runtime-defns.rkt" to-string))
(require (except-in parser-tools/lex nothing))
(require ragg/support)
(require rackunit)

(provide ast->string string->ast set-disable-srclocs!)

(define-setting DISABLE_SRCLOCS set-disable-srclocs! #f)

(define (ast->string x [keep-srcloc? (not DISABLE_SRCLOCS)])
  (aterm->string (ast->aterm x keep-srcloc?)))

(define (string->ast x)
  (aterm->ast (string->aterm x)))

(define (aterm->srcloc s os)
  (match s
    [(Node 'S (list src line col pos span))
     (srcloc src line col pos span os)]
    [(Node 'Z (list))
     (srcloc 'no-info 1 1 1 1 os)]))

(define (srcloc->aterm s)
  (Node 'S (reify-srcloc s)))

(define (ast->aterm ast keep-srcloc)
  (define-syntax-rule (node l s xs ...)
    (tagged-node s l (list xs ...)))
  (define (tagged-node s lbl xs)
    (when (not (srcloc? s))
      (error (format "bad srcloc arg: ~a ~a ~a" s lbl xs)))
    (let [[srcloc (if keep-srcloc
                      (srcloc->aterm s)
                      (Node 'Z (list)))]
          [tags (srcloc-tags s)]]
      (if (empty? tags)
          (Node lbl (cons srcloc xs))
          (Tagged tags (Node lbl (cons srcloc xs))))))
  (define (rec x) (ast->aterm x keep-srcloc))
  (define (recs xs) (List (map rec xs)))
  (define (show-name x)
    (if (symbol? x)
        (symbol->string x)
        (format "[?~a?]" x))
    #;(symbol->string x)) ;TODO(justin)
  (define (show-number x) (number->string x))
  (define (stmt s x) (if (not (s-stmt? x))
                         (Node 'Expr (list (rec x))) (rec x)))
  (define (stmts s xs) (map (λ (x) (stmt s x)) xs))
  
  (match ast
    [(? Func? x)         (rec (Func-term x))]
    ; TODO(justin)
    [(? Var? x)          (rec (Var-value x))]
    #;[(? Var? x)          (let* [[name (Var-name x)]
                                [term (rec (Var-value x))]
                                [u term #;((unexpand) term "Expr")]] ; TODO: why does (unexpand) fail with EOF here??
                           (if (or (and HIDE_UNDEFINED (undefined? u))
                                   (CouldNotUnexpand? u))
                               name
                               u))]
    ; Values
    [(s-list s xs)       (node 'List s (recs xs))]
    [(s-id s n)          (node 'Id s (show-name n))]
    [(s-num s n)         (node 'Num s (show-number n))]
    [(s-bool s b)        (node (if b 'True 'False) s)]
    [(s-str s str)       (node 'Str s str)]
    [(s-lam s ns bs a doc check block)
     (node 'Lam s (List (map show-name ns)) (recs bs)
                  (rec a) doc (rec check) (rec block))]
    [(s-method s bs a doc check block)
     (node 'Method (recs bs)
                   (rec a) doc (rec check) (rec block))]
    [(s-obj s ms)        (node 'Obj s (recs ms))]
    [(s-bind s n a)      (node 'Bind s (show-name n) (rec a))]
    ; Annotations
    [(a-blank)           (Node 'ABlank (list))]
    [(a-any)             (Node 'AAny (list))]
    [(a-name s n)        (node 'AName (show-name n))]
    [(a-arrow s as a)    (node 'AArrow s (recs as) (rec a))]
    [(a-method s as a)   (node 'AMethod s (recs as) (rec a))]
    [(a-record s fs)     (node 'ARecord s (recs fs))]
    [(a-field s str a)   (node 'AField s str (rec a))]
    [(a-pred s a x)      (node 'APred s (rec a) (rec x))]
    [(a-app s x as)
     (if (symbol? x)
         (node 'AApp s (show-name x) (recs as))
         (node 'ADotApp s (rec x) (recs as)))]
    [(a-dot s n n2)
     (node 'ADot s (show-name n) (show-name n2))]
    ; Headers
    [(s-prog s hs b)     (node 'Prog s (recs hs) (rec b))]
    [(s-import s x n)
     (if (symbol? s)
         (node 'ImportSymbol s (show-name x) (show-name n))
         (node 'ImportString s x (show-name n)))]
    [(s-provide s x)     (node 'Provide s (rec x))]
    [(s-provide-all s)   (node 'ProvideAll s)]
    ; Statements
    [(s-block s ss)      (node 'Block s (List (stmts s ss)))]
    [(s-fun s n ns bs a str check body)
     (node 'Fun s (show-name n) (List (map show-name ns)) (recs bs)
                  (rec a) str (rec check) (rec body))]
    [(s-var s b x)       (node 'Var s (rec b) (rec x))]
    [(s-let s b x)       (node 'Let s (rec b) (rec x))]
    [(s-when s x b)      (node 'When s (rec x) (rec b))]
    [(s-try s x b y)     (node 'Try s (rec x) (rec b) (rec y))]
    [(s-if s brs)        (node 'If s (recs brs))]
    [(s-if-else s brs b) (node 'IfElse s (recs brs) (rec b))]
    [(s-if-branch s x b) (node 'IfBranch s (rec x) (rec b))]
    [(s-cases s x y brs) (node 'Cases s (rec x) (rec y) (recs brs))]
    [(s-cases-else s x y brs b)
     (node 'CasesElse s (rec x) (rec y) (recs brs) (rec b))]
    [(s-cases-branch s n bs b) 
     (node 'CasesBranch s (show-name n) (recs bs) (rec b))]
    [(s-data s n ns mxs vs ms b)
     (node 'Data s (show-name n) (List (map show-name ns)) (recs mxs) (recs vs)
                   (recs ms) (rec b))]
    [(s-variant s n bs ms)
     (node 'Variant s (show-name n) (recs bs) (recs ms))]
    [(s-singleton-variant s n ms)
     (node 'SingletonVariant s (show-name n) (recs ms))]
    [(s-data-field s x y)
     (node 'DataField s (rec x) (rec y))]
    [(s-method-field s x bs a str check body)
     (node 'MethodField s (rec x) (recs bs) (rec a) str (rec check) (rec body))]
    ; Expressions
    [(s-op s op x y)
     (node 'Op s (hash-ref reverse-op-lookup-table op) (rec x) (rec y))]
    [(s-not s x)             (node 'Not s (rec x))]
    [(s-paren s x)           (node 'Paren s (rec x))]
    [(s-app s x xs)          (node 'App s (rec x) (recs xs))]
    [(s-left-app s x y ys)   (node 'LeftApp s (rec x) (rec y) (recs ys))]
    [(s-assign s n x)        (node 'Assign s (show-name n) (rec x))]
    [(s-dot s x n)           (node 'Dot s (rec x) (show-name n))]
    [(s-bracket s x y)       (node 'Bracket s (rec x) (rec y))]
    [(s-colon s x n)         (node 'Colon s (rec x) (show-name n))]
    [(s-colon-bracket s x y) (node 'ColonBracket s (rec x) (rec y))]
    [(s-for s x bs a b)      (node 'For s (rec x) (recs bs) (rec a) (rec b))]
    [(s-for-bind s b x)      (node 'ForBind s (rec b) (rec x))]
    [(s-extend s x ms)       (node 'Extend s (rec x) (recs ms))]
    ; Runtime values
    [x                       (Node 'Value (list (show-pyret-val x)))]))


(define (aterm->ast x [os (list)])
  (define (rec x) (aterm->ast x))
  (define (recs xs) (map rec (List-terms xs)))
  (define (syn s)  (aterm->srcloc s os))
  (define (read-name n) (string->symbol n))
  (define (read-names ns) (map string->symbol (List-terms ns)))
  (define (read-number n) (string->number n))

  (match x
    [(Tagged os t)
     (aterm->ast t os)]
    [(Node 'Value (list x))        (Val x)]
    ; Values
    [(Node 'List (list s xs))      (s-list (syn s) (recs xs))]
    [(Node 'Id (list s n))         (s-id (syn s) (read-name n))]
    [(Node 'Num (list s n))        (s-num (syn s) (read-number n))]
    [(Node 'True (list s))         (s-bool (syn s) #t)]
    [(Node 'False (list s))        (s-bool (syn s) #f)]
    [(Node 'Str (list s str))      (s-str (syn s) str)]
    [(Node 'Lam (list s ns bs a doc check block))
     (s-lam (syn s) (read-names ns) (recs bs) (rec a)
            doc (rec check) (rec block))]
    [(Node 'Method (list s bs a doc check block))
     (s-method (syn s) (recs bs) (rec a) (rec doc) (rec check) (rec block))]
    [(Node 'Obj (list s ms))       (s-obj (syn s) (recs ms))]
    [(Node 'Bind (list s n a))     (s-bind (syn s) (read-name n) (rec a))]
    ; Annotations
    [(Node 'ABlank (list))         (a-blank)]
    [(Node 'AAny (list))           (a-any)]
    [(Node 'AName (list s n))      (a-name (syn s) (read-name n))]
    [(Node 'AArrow (list s as a))  (a-arrow (syn s) (recs as) (rec a))]
    [(Node 'AMethod (list s as a)) (a-method (syn s) (recs as) (rec a))]
    [(Node 'ARecord (list s fs))   (a-record (syn s) (recs fs))]
    [(Node 'AField (list s str a)) (a-field (syn s) str (rec a))]
    [(Node 'APred (list s a x))    (a-pred (syn s) (rec a) (rec x))]
    [(Node 'AApp (list s n as))    (a-app (syn s) (read-name n) (recs as))]
    [(Node 'ADotApp (list s x as)) (a-app (syn s) (rec x) (recs as))]
    [(Node 'ADot (list s n n2))    (a-dot (syn s) (read-name n) (read-name n2))]
    ; Headers
    [(Node 'Prog (list s hs b))   (s-prog (syn s) (recs hs) (rec b))]
    [(Node 'ImportSymbol (list s n n2))
     (s-import (syn s) (read-name n) (read-name n2))]
    [(Node 'ImportString (list s str n))
     (s-import (syn s) str (read-name n))]
    [(Node 'Provide (list s x))   (s-provide (syn s) (rec x))]
    [(Node 'ProvideAll (list s))  (s-provide-all (syn s))]
    ; Statements
    [(Node 'Block (list s ss))     (s-block (syn s) (recs ss))]
    [(Node 'Expr (list x))         (rec x)]
    [(Node 'Fun (list s n ns bs a str check body))
     (s-fun (syn s) (read-name n) (read-names ns)
            (recs bs) (rec a) str (rec check) (rec body))]
    [(Node 'Var (list s b x))      (s-var (syn s) (rec b) (rec x))]
    [(Node 'Let (list s b x))      (s-let (syn s) (rec b) (rec x))]
    [(Node 'When (list s x b))     (s-when (syn s) (rec x) (rec b))]
    [(Node 'Try (list s x b y))    (s-try (syn s) (rec x) (rec b) (rec y))]
    [(Node 'If (list s brs))       (s-if (syn s) (recs brs))]
    [(Node 'IfElse (list s brs b)) (s-if-else (syn s) (recs brs) (rec b))]
    [(Node 'IfBranch (list s x b)) (s-if-branch (syn s) (rec x) (rec b))]
    [(Node 'Cases (list s x y brs))
     (s-cases (syn s) (rec x) (rec y) (recs brs))]
    [(Node 'CasesElse (list s x y brs b))
     (s-cases-else (syn s) (rec x) (rec y) (recs brs) (rec b))]
    [(Node 'CasesBranch (list s n bs b))
     (s-cases-branch (syn s) (read-name n) (recs bs) (rec b))]
    [(Node 'Data (list s n ns vs ms b))
     (s-data (syn s) (read-name n) (read-names ns) (recs vs) (recs ms) (rec b))]
    [(Node 'Variant (list s n bs ms))
     (s-variant (syn s) (read-name n) (recs bs) (recs ms))]
    [(Node 'SingletonVariant (list s n ms))
     (s-singleton-variant (syn s) (read-name n) (recs ms))]
    [(Node 'DataField (list s x y))
     (s-data-field (syn s) (rec x) (rec y))]
    [(Node 'MethodField (list s x bs a str check body))
     (s-method-field (syn s) (rec x) (recs bs) (rec a) str (rec check) (rec body))]
    ; Expressions
    [(Node 'Op (list s op x y))
     (s-op (syn s) (hash-ref op-lookup-table op) (rec x) (rec y))]
    [(Node 'Not (list s x))        (s-not (syn s) (rec x))]
    [(Node 'Paren (list s x))      (s-paren (syn s) (rec x))]
    [(Node 'App (list s x xs))     (s-app (syn s) (rec x) (recs xs))]
    [(Node 'LeftApp (list s x y ys)) (s-left-app (syn s) (rec x) (rec y) (recs ys))]
    [(Node 'Assign (list s n x))   (s-assign (syn s) (read-name n) (rec x))]
    [(Node 'Dot (list s x n))      (s-dot (syn s) (rec x) (read-name n))]
    [(Node 'Bracket (list s x y))  (s-bracket (syn s) (rec x) (rec y))]
    [(Node 'Colon (list s x n))    (s-colon (syn s) (rec x) (read-name n))]
    [(Node 'ColonBracket (list s x y)) (s-colon-bracket (syn s) (rec x) (rec y))]
    [(Node 'For (list s x bs a b)) (s-for (syn s) (rec x) (recs bs) (rec a) (rec b))]
    [(Node 'ForBind (list s b x))  (s-for-bind (syn s) (rec b) (rec x))]
    [(Node 'Extend (list s x ms))  (s-extend (syn s) (rec x) (recs ms))]))
    

(define (aterm->string t)
  (define (parens x) (format "(~a)" x))
  (define (braces x) (format "{~a}" x))
  (define (brackets x) (format "[~a]" x))
  (define (comma-sep xs) (string-join xs ", "))
  (define (show-string s) (format "~v" s))
  (define (show-origin o)
    (match o
      [(MacHead m c q)
       (string-append "Head"
                      (parens (comma-sep (list (symbol->string m)
                                               (number->string c)
                                               (aterm->string q)))))]
      [(MacBody) "Body"]
      [(Alien)   "Alien"]))
  (define (show-origins os)
    (braces (brackets (comma-sep (map show-origin os)))))
  (define (show-aterm t)
    (match t
      [(Node l ts)
       (string-append (symbol->string l) (parens (comma-sep (map show-aterm ts))))]
      [(List ts)
       (brackets (comma-sep (map show-aterm ts)))]
      [(Tagged os t)
       (string-append (show-aterm t) (show-origins os))]
      [(? string? t)
       (show-string t)]
      [(? integer? t)
       (number->string t)]
      [t
       (error (format "aterm->string: invalid aterm ~a" t))]))
  (show-aterm t))


(define (string->aterm str)
  (define (extract-list xs)
    (cond [(empty? xs) (list)]
          [(and (cons? xs) (string? (car xs))) (extract-list (cdr xs))]
          [(cons? xs) (cons (car xs) (extract-list (cdr xs)))]))
  (define (strip-quotes str)
    (substring str 1 (- (string-length str) 1)))
  (define (ragg->aterm x)
    (match x
      [`(number ,x)        (string->number x)]
      [(? string? x)       (strip-quotes x)]
      [`(tag "Body")       (MacBody)]
      [`(tag "Alien")      (Alien)]
      [`(tag "Head" ,_ ,l ,_ ,i ,_ ,t ,_)
       (MacHead (string->symbol l) (string->number i) (ragg->aterm t))]
      [`(tags . ,xs)       (map ragg->aterm (extract-list xs))]
      [`(terms . ,xs)      (map ragg->aterm (extract-list xs))]
      [`(list ,_ ,xs ,_)   (List (ragg->aterm xs))]
      [`(node ,l ,_ ,x ,_) (Node (string->symbol l) (ragg->aterm x))]
      [`(term ,x)          (ragg->aterm x)]
      [`(term ,x ,o)       (Tagged (ragg->aterm o) (ragg->aterm x))]))
  
  (ragg->aterm (syntax->datum (parse (tokenize (open-input-string str))))))


(define (tokenize file)
  ; Glommed together from Danny Yoo's Ragg example & pyret lang lexer
  (port-count-lines! file)
  (define lexer
    (lexer-src-pos
     ;; numbers
     [(concatenation
       (repetition 1 +inf.0 numeric)
       (union ""
              (concatenation
               #\.
               (repetition 1 +inf.0 numeric))))
      (token 'NUMBER lexeme)]
     ;; strings
     [(concatenation
       "\""
       (repetition 0 +inf.0 (union "\\\"" (intersection
                                           (char-complement #\")
                                           (char-complement #\newline))))
       "\"")
      (token 'STRING lexeme)] ; escaping?
     ;; tags
     [(union "Head" "Body" "Alien")
      (token lexeme lexeme)]
     ;; brackets
     [(union "[" "]" "{" "}" "(" ")" ",")
      (token lexeme lexeme)]
     ;; whitespace
     [whitespace
      (token 'WS lexeme #:skip? #t)]
     ;; labels
     [(repetition 1 +inf.0 alphabetic)
      (token 'LABEL lexeme)]
     [(eof) (void)]))
  (lambda () (lexer file)))


(define (test-conversion x)
  (check-equal? x (string->ast (ast->string x))))

