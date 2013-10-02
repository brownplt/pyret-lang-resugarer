#lang racket/base

(require
  racket/match
  racket/string
  racket/function
  racket/list
  "ast.rkt"
  "stepper/data.rkt"
  (only-in "runtime-defns.rkt" to-string))

(provide
  pretty
  pretty-ann)

(define tab-size 2)

(define (vary-pretty ast ind)
  (define (pretty ast) (vary-pretty ast ind))
  (define (prettier ast) (vary-pretty ast (+ ind 1)))
  (define (indent [depth 1]) (make-string (* tab-size depth) #\space))
  (define (indented str [depth 1])
    (if str (format "~a~a" (indent depth) str) #f))
  (define (wrap l r center) (format "~a~a~a" l center r))
  (define (parens x) (wrap "(" ")" x))
  (define (brackets x) (wrap "[" "]" x))
  (define (angles x) (wrap "<" ">" x))
  (define (braces x) (wrap "{" "}" x))
  (define (sep delim parts)
    (string-join (filter (lambda (x) x) parts) delim))
  (define (comma-sep xs) (sep ", " xs))
  (define (spaces . xs) (sep " " xs))
  (define (concat . xs) (sep "" xs))
  (define (newlines . xs) (sep (format "\n~a" (indent ind)) xs))

  (define (pretty-return-ann ann)
    (if (a-blank? ann) #f (format "-> ~a" (pretty-ann ann))))
  
  (define (pretty-doc doc)
    (if (equal? "" doc) #f (format "doc: \"~a\"" doc)))
  
  (define (pretty-fun-header name params args ann)
    (spaces
     (if (empty? params) #f (angles (comma-sep (map pretty params))))
     (concat (pretty name)
             (parens (comma-sep (map pretty args))))
     (pretty-return-ann ann)))
  
  (define (pretty-method-header args ann)
    (spaces
     (parens (comma-sep (map pretty args)))
     (pretty-return-ann ann)))

  (define (pretty-check block)
    (if (empty? (s-block-stmts block)) #f
        (newlines "check:"
                  (indented (prettier block)))))
  
  (match ast
    
    [(? Val? ast) (to-string (Val-value ast))]
    
    [(? Var? ast) (pretty (Var-value ast))] ;TODO
    
    [(? Func? ast) (pretty (Func-term ast))]
    
    [(? string? ast) (format "~v" ast)]
    
    [(? symbol? ast) (symbol->string ast)]
    
    [(s-prog _ imps block)
     (apply newlines (append (map pretty imps) (list (pretty block))))]
    
    [(s-block _ stmts)
     (apply newlines (map pretty stmts))]
    
    [(s-let _ bnd val)
     (format "~a = ~a" (pretty bnd) (pretty val))]
    
    [(s-var _ bnd val)
     (format "var ~a = ~a" (pretty bnd) (pretty val))]
    
    [(s-bind _ id ann)
     (if (a-blank? ann)
         (pretty id)
         (format "~a :: ~a" (pretty id) (pretty-ann ann)))]
    
    [(s-fun _ name params args ann doc body check)
     (newlines (format "fun ~a:" (pretty-fun-header name params args ann))
               (indented (pretty-doc doc))
               (indented (prettier body))
               (pretty-check check)
               "end")]
    
    [(s-method _ args ann doc body check)
     (newlines (format "method~a:" (pretty-method-header args ann))
               (indented (pretty-doc doc))
               (indented (prettier body))
               (indented (prettier check))
               "end")]
    
    [(s-lam _ params args ann doc body check)
     (spaces (pretty-fun-header "fun" params args ann)
             (pretty-doc doc)
             (prettier body)
             (pretty-check check)
             "end")]
    
    [(s-assign s name expr)
     (format "~a := ~a" name (pretty expr))]
    
    [(s-when _ cond consq)
     (newlines (format "when ~a:" (pretty cond))
               (indented (prettier consq)))]
    
    [(s-if _ (cons (s-if-branch _ cond consq) brs))
     (newlines (format "if ~a:" (pretty cond))
               (indented (prettier consq))
               (if (empty? brs) #f (apply newlines (map pretty brs)))
               "end")]
    
    [(s-if-else _ (cons (s-if-branch _ cond consq) brs) else)
     (newlines (format "if ~a:" (pretty cond))
               (indented (prettier consq))
               (if (empty? brs) #f (apply newlines (map pretty brs)))
               "else:"
               (indented (prettier else))
               "end")]

    [(s-if-branch _ cond consq)
     (newlines (format "else if ~a:" (pretty cond))
               (indented (prettier consq)))]
    
    [(s-for _ iter binds ann body)
     (newlines (concat "for "
                       (pretty iter)
                       (parens (comma-sep (map pretty binds)))
                       (pretty-return-ann ann)
                       ":")
               (indented (prettier body)))]
    
    [(s-for-bind _ bind body)
     (format "~a from ~a" (pretty bind) (pretty body))]
    
    [(s-list _ elts)
     (brackets (comma-sep (map pretty elts)))]
    
    [(s-obj _ fields)
     (braces (comma-sep (map pretty fields)))]
    
    [(s-extend _ expr fields)
     (format "~a.~a" (pretty expr) (braces (comma-sep (map pretty fields))))]
    
    [(s-data-field _ name value)
     (format "~a : ~a" (pretty name) (pretty value))]
    
    ; TODO: method-field
    
    [(s-app _ fun args)
     (format "~a(~a)" (pretty fun) (comma-sep (map pretty args)))]
    
    [(s-left-app _ obj fun args)
     (concat (pretty obj) "^" (pretty fun) (parens (comma-sep (map pretty args))))]
    
    [(s-extend _ super fields)
     (format "~a.{ ~a }"
             (pretty super)
             (comma-sep (map pretty fields)))]
    
    [(s-obj _ fields)
     (format "{ ~a }"
             (comma-sep (map pretty fields)))]
    
    [(s-op _ op e1 e2)
     (format "~a ~a ~a" (pretty e1) (substring (symbol->string op) 2) (pretty e2))]
    
    [(s-not _ e)
     (format "not ~a" (pretty e))]
    
    [(s-dot _ val field)
     (format "~a.~a" (pretty val) field)] 
    
    [(s-bracket _ val field)
     (format "~a.[~a]" (pretty val) (pretty field))]
    
    [(s-colon _ obj field)
     (format "~a:~a" (pretty obj) field)]
    
    [(s-colon-bracket _ obj field)
     (format "~a:[~a]" (pretty obj) (pretty field))]

    [(s-num _ n) (number->string n)]
    [(s-bool _ #t) "true"]
    [(s-bool _ #f) "false"]
    [(s-str _ s) (format "\"~a\"" s)]
    [(s-id _ id) (symbol->string id)]

    [(s-paren _ e) (format "(~a)" (pretty e))]
    
    [val (to-string val)]))
  
#|
  (define (pretty-member ast)
    (match ast
      [(s-data-field s name value)
       (format "[~a] : ~a" (pretty name) (pretty value))]))
  (define (pretty-if-branch br ind)
    (format "~aelse if ~a:\n~a~a\n"
            indent
            (next-pretty (s-if-branch-expr br))
            next-indent
            (next-pretty (s-if-branch-body br))))
  (define (pretty-if-branches brs ind)
    (string-join (map (λ (br) (pretty-if-branch br ind)) brs) "\n"))
  (match ast
    [(s-block s stmts)
     (define strs (map pretty stmts))
     (string-join strs (format "\n~a" indent))]
    [(s-if _ (cons (s-if-branch _ cond consq) brs))
     (format "if ~a:\n~a~a\n~a~aend"
             (next-pretty cond)
             next-indent
             (next-pretty consq)
             (pretty-if-branches brs ind)
             indent)]
    [(s-if-else _ (cons (s-if-branch _ cond consq) brs) else)
     (format "if ~a:\n~a~a\n~a~aelse:\n~a~a\n~aend"
             (next-pretty cond)
             next-indent
             (next-pretty consq)
             (pretty-if-branches brs ind)
             indent
             next-indent
             (next-pretty else)
             indent)]
    [(s-lam s typarams args ann doc body check)
     (define s-typarams
       (cond [(cons? typarams) (format "<~a>" (string-join (map symbol->string typarams) ", "))]
             [(empty? typarams) ""]))
     (define s-args (string-join (map pretty-bind args) ", "))
     (define s-ann (pretty-ann ann))
     (define s-body (vary-pretty (s-block s (cons (s-str s doc) (s-block-stmts body))) (increase-indent ind)))
     ;; NOTE(dbp): pretty printing for check is almost certainly wrong
     (define s-check (pretty check))
     (format "\\~a ~a -> ~a:\n~a~a\n~acheck~aend"
             s-typarams
             s-args
             s-ann
             next-indent
             s-body
             indent
             s-check)]
    
    [(s-method s args ann doc body check)
     (define s-args (string-join (map pretty-bind args) ", "))
     (define s-ann (pretty-ann ann))
     (define s-body (pretty body))
     (define s-check (pretty check))
     (format "method(~a) -> ~a: ~a check ~a end" s-args s-ann s-body s-check)]

    [(s-assign s name expr)
     (format "~a := ~a" name (pretty expr))]

    [(s-app s fun args)
     (format "~a(~a)" (pretty fun) (commas (map pretty args)))]

    [(s-extend s super fields)
     (format "~a.{ ~a }"
             (pretty super)
             (string-join (map pretty-member fields) ", "))]

    [(s-obj s fields)
     (format "{ ~a }"
             (string-join (map pretty-member fields) ", "))]

    [(s-op s op e1 e2)
     (format "~a ~a ~a" (pretty e1) (substring (symbol->string op) 2) (pretty e2))]
    
    [(s-dot s val field)
     (format "~a.~a" (pretty val) field)] 
    
    [(s-bracket s val field)
     (format "~a.[~a]" (pretty val) (pretty field))]
    
    [(s-colon s obj field)
     (format "~a:~a" (pretty obj) field)]
    
    [(s-colon-bracket s obj field)
     (format "~a:[~a]" (pretty obj) (pretty field))]

    [(s-num _ n) (number->string n)]
    [(s-bool _ #t) "true"]
    [(s-bool _ #f) "false"]
    [(s-str _ s) (format "\"~a\"" s)]
    [(s-id _ id) (symbol->string id)]

    [(s-paren _ e) (format "(~a)" (pretty e))]

    [else "<unprintable-expr>"]))
|#

(define (pretty-ann ann)
  (match ann
    [(a-name _ id) (symbol->string id)]
    [(a-dot _ obj fld) (string-join (list (symbol->string obj)
                                          "."
                                          (symbol->string fld)) "")]
    [(a-arrow _ t1 t2) (format "(~a -> ~a)" (string-join (map pretty-ann t1) ", ") (pretty-ann t2))]
    [(a-method _ t1 t2) (format "(~a => ~a)" (string-join (map pretty-ann t1) ", ") (pretty-ann t2))]
    [(a-blank) "Any"]
    [(a-any) "Any"]
    [(a-app _ base args) (format "~a<~a>" (pretty-ann base) (string-join (map pretty-ann args) ", "))]
    [(a-pred _ ann expr) (format "~a(~a)" (pretty-ann ann) (pretty expr))]))

(define (pretty ast) (vary-pretty ast 0))
