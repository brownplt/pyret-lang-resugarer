#lang racket/base

(require
  racket/match
  racket/string
  racket/function
  racket/list
  "ast.rkt"
  "stepper/data.rkt"
  (only-in "runtime-defns.rkt" p-opaque? p-base? p-str? p-str-s to-repr))

(provide
  pretty
  pretty-ann
  pretty-val)

(define (pretty ast) (vary-pretty ast 0))

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
  (define (newlines-prefixed . xs) (sep "\n" (map (lambda (x) (format "~a~a" (indent ind) x)) xs)))

  (define (pretty-return-ann ann)
    (if (a-blank? ann) #f (format "-> ~a" (pretty-ann ann))))
  
  (define (pretty-doc doc)
    (if (equal? "" doc) #f (format "doc: \"~a\"" doc)))

  (define (pretty-fun-header name params args ann)
    (spaces
     (if (empty? params) #f (angles (comma-sep (map pretty params))))
     (concat name
             (parens (comma-sep (map pretty args)))
             ":")
     (pretty-return-ann ann)))

  (define (pretty-method-header args ann)
    (spaces
     (parens (comma-sep (map pretty args)))
     (pretty-return-ann ann)))

  (define (pretty-check block)
    (if (empty? (s-block-stmts block)) #f
        (newlines "check:"
                  (indented (prettier block)))))
  
  (define (pretty-implicit-block block)
    (if (s-block? block)
        (apply newlines-prefixed (map pretty (s-block-stmts block)))
        (pretty block)))
  
  (define (prettier-implicit-block block)
    (if (s-block? block)
        (apply newlines-prefixed (map prettier (s-block-stmts block)))
        (prettier block)))

  (define (pretty-subexp x)
    ;;; When 'x' is a stmt, wrap it in parens for clarity.
    (define (is-stmt? x)
      (match x
        [(? Val? x)     (is-stmt? (Val-value x))]
        [(? Var? x)     (is-stmt? (Var-value x))]
        [(? Func? x)    (is-stmt? (Func-term x))]
        [(? s-stmt? x)  #t]
        [(? s-cases? x) #t]
        [_              #f]))
    (if (is-stmt? x) (format "(~a)" (pretty x)) (pretty x)))
  
  (match ast
    
    ; Resugarer-specific:
    [(? Val? ast) (Val-value ast)]
    [(? Var? ast) (pretty (Var-value ast))] ;TODO
    [(? Func? ast) (pretty (Func-term ast))]
    ; End
    
    [(? string? ast) (format "~v" ast)]
    
    [(? symbol? ast) (symbol->string ast)]
    
    [(s-prog _ imps block)
     (apply newlines
        (append
         (map pretty imps)
         (list (pretty-implicit-block block))))]
    
    ; Flatten uncessarily nested blocks
    [(s-block _ (list (s-block s stmts)))
     (pretty (s-block s stmts))]
    
    [(s-block _ stmts)
     (apply newlines (append (list "block:")
                             (map indented (map prettier stmts))
                             (list "end")))]
    
    [(s-user-block _ (s-block _ stmts))
     (apply newlines (append (list "block:")
                             (map indented (map prettier stmts))
                             (list "end")))]
    
    [(s-user-block _ x)
     (apply newlines (append (list "block:")
                             (list (indented (pretty x)))
                             (list "end")))]
    
    [(s-let _ bnd val)
     (format "~a = ~a" (pretty bnd) (pretty val))]
    
    [(s-var _ bnd val)
     (format "var ~a = ~a" (pretty bnd) (pretty val))]
    
    [(s-bind _ id ann)
     (if (a-blank? ann)
         (pretty id)
         (format "~a :: ~a" (pretty id) (pretty-ann ann)))]
    
    [(s-fun _ name params args ann doc body check)
     (newlines (format "fun ~a:" (pretty-fun-header (symbol->string name) params args ann))
               (indented (pretty-doc doc))
               (indented (prettier-implicit-block body))
               (pretty-check check)
               "end")]
    
    [(s-method _ args ann doc body check)
     (newlines (format "method~a:" (pretty-method-header args ann))
               (indented (pretty-doc doc))
               (indented (prettier-implicit-block body))
               (indented (prettier check))
               "end")]
    
    [(s-lam _ params args ann doc body check)
     (spaces (pretty-fun-header "fun" params args ann)
             (pretty-doc doc)
             (prettier-implicit-block body)
             (pretty-check check)
             "end")]
    
    [(s-assign s name expr)
     (format "~a := ~a" name (pretty expr))]
    
    [(s-when _ cond consq)
     (newlines (format "when ~a:" (pretty cond))
               (indented (prettier consq)))]
    
    [(s-if _ (cons (s-if-branch _ cond consq) brs))
     (newlines (format "if ~a:" (pretty cond))
               (indented (prettier-implicit-block consq))
               (if (empty? brs) #f (apply newlines (map pretty brs)))
               "end")]
    
    [(s-if-else _ (cons (s-if-branch _ cond consq) brs) else)
     (newlines (format "if ~a:" (pretty cond))
               (indented (prettier-implicit-block consq))
               (if (empty? brs) #f (apply newlines (map pretty brs)))
               "else:"
               (indented (prettier-implicit-block else))
               "end")]

    [(s-if-branch _ cond consq)
     (newlines (format "else if ~a:" (pretty cond))
               (indented (prettier-implicit-block consq)))]
    
    [(s-try _ x (s-bind _ v _) y)
     (newlines "try:"
               (indented (prettier-implicit-block x))
               (format "except(~a):" v)
               (indented (prettier-implicit-block y))
               "end")]
    
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
    
    [(s-cases _ a e bs)
     (newlines
      (format "cases(~a) ~a:" (pretty-ann a) (pretty e))
      (apply newlines (map indented (map pretty bs)))
      "end")]

    [(s-cases-else _ a e bs else)
     (newlines
      (format "cases(~a) ~a:" (pretty-ann a) (pretty e))
      (apply newlines (map indented (map pretty bs)))
      (indented (format "| else => ~a" (pretty-implicit-block else)))
      "end")]

    
    [(s-cases-branch _ c bs e)
     (format "| ~a(~a) => ~a"
             (pretty c)
             (comma-sep (map pretty bs))
             (pretty-implicit-block e))]
    
    ; TODO: method-field
    
    [(s-app _ fun args)
     (format "~a(~a)" (pretty-subexp fun) (comma-sep (map pretty args)))]
    
    [(s-left-app _ obj fun args)
     (concat (pretty obj) "^" (pretty-subexp fun) (parens (comma-sep (map pretty args))))]
    
    [(s-extend _ super fields)
     (format "~a.{ ~a }"
             (pretty-subexp super)
             (comma-sep (map pretty fields)))]
    
    [(s-obj _ fields)
     (format "{ ~a }"
             (comma-sep (map pretty fields)))]
    
    [(s-op _ op e1 e2)
     (format "~a ~a ~a" (pretty-subexp e1) (substring (symbol->string op) 2) (pretty-subexp e2))]
    
    [(s-not _ e)
     (format "not ~a" (pretty-subexp e))]
    
    [(s-dot _ val field)
     (format "~a.~a" (pretty-subexp val) field)] 
    
    [(s-bracket _ val field)
     (format "~a.[~a]" (pretty-subexp val) (pretty field))]
    
    [(s-colon _ obj field)
     (format "~a:~a" (pretty-subexp obj) field)]
    
    [(s-colon-bracket _ obj field)
     (format "~a:[~a]" (pretty-subexp obj) (pretty field))]

    [(s-num _ n) (number->string n)]
    [(s-bool _ #t) "true"]
    [(s-bool _ #f) "false"]
    [(s-str _ s) (format "\"~a\"" s)]
    [(s-id _ id) (symbol->string id)]

    [(s-paren _ e) (format "(~a)" (pretty e))]
    
    [else (pretty-val ast)]))

(define (pretty-val val)
  (cond [(p-opaque? val) val]
        [(p-str? val)
         (format "'~a'" (p-str-s val))]
        [(p-base? val) (to-repr val)]
        [(procedure? val) "func"]
        [else "unprintable-expr"]))

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
    [(a-pred _ ann expr) (format "~a(~a)" (pretty-ann ann) (pretty expr))]
    [(a-record _ fields) (format "{~a}" (string-join
                                         (map (λ(f) (format "~a: ~a"
                                                            (a-field-name f)
                                                            (pretty-ann (a-field-ann f))))
                                              fields) ", "))]))
