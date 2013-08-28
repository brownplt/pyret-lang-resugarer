#lang racket

(provide
  compile-pyret
  compile-expr
  ; for testing:
  adorn)
(require
  racket/match
  racket/splicing
  racket/syntax
  "ast.rkt"
  "runtime.rkt"
  "compile-helpers/find.rkt"
  "compile-helpers/lift-constants.rkt")
  
  
;;; Keeping track of the stack ;;;

(define (compile-stepping-prelude stx resugar)
  (if resugar
      (with-syntax [[code* stx]]
        #'(resugarer:with-resugaring "src/lang/stepper"
           (r:let []
            (r:define $emit (r:lambda (z)
               (resugarer:emit z)))
            code*)))
      stx))

;;; Stepper ;;;

; Push a frame onto the stack (and pop it after)
(define (annot/frame expr_ frame_)
  (with-syntax [[expr* expr_]
                [frame* (make-frame frame_)]]
    #'(r:begin ($push! frame*)
               ($set-val! expr*)
               ($pop!)
               $val)))

(define (make-frame body_)
  (with-syntax [[body* body_]]
    #'(r:lambda (__) body*)))

#|
; Annotate function argument expressions
(define (annot/args xs_ fv_ xvs0_ xvs_ xts_)
  (if (empty? xs_)
      empty
      (with-syntax [[fv* fv_]
                    [(xvs0* ...) xvs0_]
                    [(xts* ...) (cdr xts_)]]
        (cons (annot/frame (annot/eval (car xs_))
                           #'(r:list fv* xvs0* ... __ xts* ...))
              (annot/args (cdr xs_) fv_ (append xvs0_ (list (car xvs_)))
                          (cdr xvs_) (cdr xts_))))))
|#

; Call external code
(define (annot/extern-call func_ args_)
  (error "annot/extern-call NYI"))

; Call a function, which may or may not have been annotated
(define (annot/call func_ args_)
  (error "annot/call NYI"))

; Prepare a term to be shown.
(define (adorn x)
  (define (struct-name->constr struct-name)
    ; strip off "struct:"
    (let [[name (string->symbol (substring (symbol->string struct-name) 7))]]
      #`#,name))
  (cond [(or (string? x) (boolean? x) (number? x))
         #`#,x]
        [(symbol? x)
         #'(r:quote x)]
        [(list? x)
         (with-syntax [[(xs* ...) (map adorn x)]]
           #'(r:list xs* ...))]
        [(s-id? x)
         (with-syntax [[v* (s-id-id x)]]
           #'(s:Var (r:quote v*) (r:quote v*)))] ; TODO(justin)
        [(info? x)
         (with-syntax [[loc* (adorn (info-loc x))]
                       [(tags* ...) (info-tags x)]]
           #'(s:info loc* (r:list tags* ...)))]
        [(srcloc? x)
         (match (reify-srcloc x)
           [(list src line col pos span)
            #`(s:srcloc #,src #,line #,col #,pos #,span)])]
        [(struct? x)
         (let* [[children (vector->list (struct->vector x))]
                [constr (struct-name->constr (car children))]
                [subnodes (cdr children)]]
           #`(#,constr #,@(map adorn subnodes)))]
        [else
         (error (format "stepper/adorn: Unrecognized AST node type. ~a" x))]))


;;; Compilation ;;;
  
(define (loc-list info)
  (define loc (info-loc info))
  (define (serialize-source e)
    (cond
      [(symbol? e) (symbol->string e)]
      [(string? e) e]
      [(path? e) (path->string e)]
      [(false? e) "unknown source"]
      [else (error (format "Non-symbol, non-string, non-path value for
                            source: ~a" e))]))
  (list (serialize-source (srcloc-source loc))
        (srcloc-line loc)
        (srcloc-column loc)
        (srcloc-position loc)
        (srcloc-span loc)))

(define (loc-stx loc)
  (with-syntax ([(loc-param ...) (loc-list loc)])
    #'(r:list loc-param ...)))

(define (args-stx l args)
  (d->stx (map discard-_ (map s-bind-id args)) l))

(struct compile-env (functions-to-inline toplevel?) #:transparent)

(define (d->stx stx loc) (datum->syntax #f stx (loc-list loc)))

(define (attach loc stx)
  (datum->syntax #f (syntax-e stx) (loc-list loc)))

(define (block-fun-ids stmts)
  (define (stmt-id stmt)
    (match stmt
      [(s-let _ (s-bind _ id _) (s-lam _ _ _ _ _ _ _)) id]
      [(s-let _ (s-bind _ id _) (s-extend s (s-lam _ _ _ _ _ _ _) fields)) id]
      [_ #f]))
  (list->set (filter-map stmt-id stmts)))

(define (block-ids stmts)
  (define (stmt-id stmt)
    (match stmt
      [(s-let _ (s-bind _ id _) _) id]
      [(s-var _ (s-bind _ id _) _) id]
      [_ #f]))
  (filter-map stmt-id stmts))

(define (make-immediate-id id)
  (string->symbol (format "~a##inline" id)))

(define (compile-block l stmts env resugar)
  (define (compile-expr expr env) (compile-expr/internal expr env resugar))
  (define (compile-stmt ast-node env add-frame)
    (match ast-node
      [(s-var s (s-bind _ id _) val)
        (list 
          #`(r:define #,(discard-_ id)
                      #,(add-frame (compile-expr val env))))]
      [(s-let s (s-bind _ id _) val)
       (define (match-id-use e)
        (match e
          [(s-app s (s-id s2 (? (lambda (x) (equal? id x)) x)) args)
           (s-id s2 x)]
          [(s-id s (? (lambda (x) (equal? id x)) x))
           (s-id s x)]
          [_ #f]))
       (define ids (find (s-block l stmts) match-id-use))
       (define id-used (or (> (length (remove-duplicates ids)) 1)
                           (= (length ids) 1)))
       (match val
        [(s-lam l _ args _ doc body _)
         (define inline-binding
          (with-syntax ([(arg ...) (args-stx l args)])
            #`(r:define
               #,(make-immediate-id id)
               #,(add-frame #`(p:arity-catcher
                               (arg ...)
                               #,(compile-expr body env))))))
         (cond
          [(or (compile-env-toplevel? env) id-used)
            (list inline-binding
                  (with-syntax ([(arg ...) (args-stx l args)]
                                [f-id (make-immediate-id id)])
                    #`(r:define
                       #,(discard-_ id)
                       #,(add-frame
                          #`(p:pλ (arg ...) #,doc (f-id arg ...))))))]
          [else (list inline-binding)])]
        [(s-extend s (s-lam l _ args _ doc body _) fields)
         (define inline-binding
          (with-syntax ([(arg ...) (args-stx l args)])
            #`(r:define #,(make-immediate-id id)
               #,(add-frame #`(p:arity-catcher
                               (arg ...)
                               #,(compile-expr body env))))))
         (cond
          [(or (compile-env-toplevel? env) id-used)
            (list inline-binding
                  (with-syntax ([(arg ...) (args-stx l args)]
                                [f-id (make-immediate-id id)]
                                [(field ...)
                                 (map (λ (mem) (compile-member mem env resugar))
                                      fields)])
                    #`(r:define #,(discard-_ id)
                     #,(add-frame
                       #`(p:extend
                          #,(loc-stx s)
                          (p:pλ (arg ...) #,doc (f-id arg ...))
                          (r:list field ...))))))]
           [else (list inline-binding)])]
        [_ (list #`(r:define #,(discard-_ id)
                    #,(add-frame (compile-expr val env))))])]
      [_ (list (add-frame (compile-expr ast-node env)))]))
  (define (compile-stmts stmts env)
    (if (empty? stmts)
        (list)
        (let* [[fr #`(s-block
                      #,l (r:list __ #,@(map adorn (cdr stmts))))]
               [add-frame (λ (stx) (frame resugar fr stx))]]
          (append (compile-stmt (car stmts) env add-frame)
                  (compile-stmts (cdr stmts) env)))))
  (define ids (block-ids stmts))
  (define fun-ids (block-fun-ids stmts))
  (define old-fun-ids (compile-env-functions-to-inline env))
  (define avoid-shadowing (set-subtract old-fun-ids (list->set ids)))
  (define new-env (compile-env (set-union avoid-shadowing fun-ids)
                               (compile-env-toplevel? env)))
  (define stmts-stx (compile-stmts stmts new-env))
  #;(define stmts-stx (append* (map (curryr compile-stmt new-env) stmts)))
  (if (empty? stmts-stx) (list #'nothing) stmts-stx))

(define (compile-member ast-node env resugar)
  (match ast-node
    [(s-data-field l name value)
     (attach l
       (with-syntax*
        ([name-stx (compile-string-literal l name env resugar)]
         [val-stx (compile-expr/internal value env resugar)]) 
         #`(r:cons name-stx val-stx)))]))
(define (compile-string-literal l e env resugar)
  (match e
    [(s-str _ s) (d->stx s l)]
    [else #`(p:check-str #,(compile-expr/internal e env resugar) #,(loc-stx l))]))

(define (mark l expr)
  (with-syntax [((loc-param ...) (loc-list l))]
    #`(r:with-continuation-mark (r:quote pyret-mark) (r:srcloc loc-param ...) #,expr)))

(define (frame resugar fr expr)
  (if resugar
      #`(r:with-continuation-mark
         (r:quote resugar-mark)
         (r:lambda (__) #,fr)
         (r:begin
          (r:let [[result (r:let [] #,expr)]]
                 ($emit result)
                 result)))
      #`#,expr))

(define (compile-expr/internal ast-node env resugar)
  (let [[compile-member (λ (mem env) (compile-member mem env resugar))]]
  (define compile-expr (λ (expr env) (compile-expr/internal expr env resugar)))
  (define (compile-body l body new-env)
    (mark l (compile-expr body new-env)))
  (define (compile-lookup l obj field lookup-type)
     (attach l
      (with-syntax*
         ([field-stx (compile-string-literal l field env resugar)])
       #`(#,lookup-type #,(loc-stx l) #,(compile-expr obj env) field-stx))))
  (match ast-node
    
    [(s-block l stmts)
     (define new-env (compile-env (compile-env-functions-to-inline env) #f))
     (with-syntax ([(stmt ...) (compile-block l stmts new-env resugar)])
       (attach l #'(r:let () stmt ...)))]

    [(s-num l n) #`(p:mk-num #,(d->stx n l))]
    [(s-bool l #t) #`p:p-true]
    [(s-bool l #f) #`p:p-false]
    [(s-str l s) #`(p:mk-str #,(d->stx s l))]

    [(s-lam l params args ann doc body _)
     (define new-env (compile-env (compile-env-functions-to-inline env) #f))
     (attach l
       (with-syntax ([(arg ...) (args-stx l args)]
                     [body-stx (compile-body l body new-env)])
         #`(p:pλ (arg ...) #,doc body-stx)))]
    
    [(s-method l args ann doc body _)
     (define new-env (compile-env (compile-env-functions-to-inline env) #f))
     (attach l
       (with-syntax ([(arg ...) (args-stx l args)]
                     [body-stx (compile-body l body new-env)])
         #`(p:pμ (arg ...) #,doc body-stx)))]

    [(s-if-else l c-bs else-block)
     (define (compile-if-branch b)
       (match b
         [(s-if-branch s test block)
          (attach l
                  #`((p:pyret-true? #,(compile-expr test env))
                     #,(compile-expr block env)))]))
     (attach l
        (if (not resugar)
     ; w/o resugar:
     (with-syntax ([(branch ...) (d->stx (map compile-if-branch c-bs) l)])
       #`(r:cond branch ... [#t #,(compile-expr else-block env)]))
     (match c-bs
     ; w/ resugar:
     [(list)
      (compile-expr else-block env)]
     [(cons (s-if-branch s test block) brs)
      #`(r:if
         #,(mark (empty-info 'if1)
         #`(p:pyret-true?
          #,(frame resugar
             #`(s-if-else
                #,l
                #,(mark (empty-info 'if11)
                   #`(r:cons (s-if-branch #,l __ #,(adorn block))
                        (r:list #,@(map adorn brs))))
                #,(mark (empty-info 'if12) (adorn else-block)))
             (compile-expr test env))))
         #,(mark (empty-info 'if2) (compile-expr block env))
         #,(mark (empty-info 'if3)
                 (compile-expr (s-if-else l (cdr c-bs) else-block) env)))])))]
      #;(with-syntax [[(v*) (generate-temporaries #'(var))]]
        #`(r:let [[v* #,(annot/frame
                        (compile-expr test env)
                        #`(s-if-else
                           #,l
                           (r:cons (s-if-branch #,l __ #,(adorn block))
                                   (list #,@(map adorn brs)))
                           #,(adorn else-block)))]]
                 #,(mark (empty-info 'if38) #`($emit (s-if-else
                         #,l
                         (r:cons (s-if-branch #,l v* #,(adorn block))
                                 (r:list #,@(map adorn brs)))
                         #,(adorn else-block))))
              (r:if (p:pyret-true? v*)
                  #,(compile-expr block env)
                  #,(compile-expr (s-if-else l (cdr c-bs) else-block) env))));])))]
    
    [(s-try l try (s-bind l2 id ann) catch)
     (attach l
       #`(r:with-handlers
            ([p:exn:fail:pyret?
              (r:lambda (%exn)
               (r:define #,(d->stx (discard-_ id) l2) (p:mk-exn %exn))
               #,(compile-expr catch env))])
            #,(compile-expr try env)))]

    [(s-id l name)
     (attach l
       (with-syntax ([name-stx (d->stx (discard-_ name) l)])
         #'name-stx))]

    [(s-assign l name expr)
     (attach l
       (with-syntax ([name-stx (d->stx name l)]
                     [temp (gensym name)])
         #`(r:let [(temp #,(compile-expr expr env))]
             (r:set! name-stx temp)
             temp)))]

    [(s-app l (s-bracket l2 obj field) args)
     (with-syntax* ([obj (compile-expr obj env)]
                    [(arg ...) (map (curryr compile-expr env) args)]
                    [(argid ...) (map (λ (_) (format-id #'obj "~a" #`#,(gensym 'arg))) args)]
                    [field (compile-string-literal l2 field env resugar)])
         (mark l
          #`(r:let* ([%obj obj]
                     [%field (p:get-raw-field #,(loc-stx l) %obj field)]
                     [argid arg] ...)
              ((p:p-base-method %field) %obj argid ...))))]


    [(s-app l fun args)
     (define (compile-fun-expr fun)
      (match fun
        [(s-id l2 (? (λ (s) (set-member? (compile-env-functions-to-inline env) s)) id))
         (make-immediate-id id)]
        [(s-lam l _ args _ doc body _)
         (with-syntax ([(arg ...) (args-stx l args)])
           #`(p:arity-catcher (arg ...) #,(compile-expr body env)))]
        [_ #`(p:p-base-app #,(compile-expr fun env))]))
     (attach l
        (with-syntax ([fun (compile-fun-expr fun)]
                      [(arg ...) (map (curryr compile-expr env) args)])
          (mark l #'(fun arg ...))))]
    ;; !!! insertion point here - r:begin, (s:emit term $stk)

    [(s-obj l fields)
     (attach l
       (with-syntax ([(member ...) (map (curryr compile-member env) fields)])
         #'(p:mk-object (p:make-string-map (r:list member ...)))))]
    
    [(s-extend l super fields)
     (attach l
       (with-syntax ([(member ...) (map (curryr compile-member env) fields)]
                     [super (compile-expr super env)])
        #`(p:extend #,(loc-stx l)
                    super
                    (r:list member ...))))]
    
    [(s-bracket l obj field)
     (compile-lookup l obj field #'p:get-field)]
    
    [(s-colon-bracket l obj field)
     (compile-lookup l obj field #'p:get-raw-field)]

    [(s-prog l headers block) (compile-prog l headers block resugar)]

    [else (error (format "Missed a case in compile: ~a" ast-node))])))

(define (compile-header header resugar)
  (match header
    [(s-import l file name)
     (attach l
       (with-syntax
        ([file-stx file])
       (with-syntax
         ([name-stx name]
          [req-stx (if (relative-path? file) #'file-stx #'(r:file file-stx))])
        #`(r:require (r:rename-in req-stx [%PYRET-PROVIDE name-stx])))))]

    [(s-provide l exp)
     (attach l
      (with-syntax [(temp-stx (gensym 'module-provide))]
        #`(r:begin
            (r:define temp-stx #,(compile-expr exp resugar))
            (r:provide (r:rename-out [temp-stx %PYRET-PROVIDE])))))]))



(define (compile-prog l headers block resugar)
  (define (compile-hdr header) (compile-header header resugar))
  (attach l
   (with-syntax ([(req ...) (map compile-hdr (filter s-import? headers))]
                 [(prov ...) (map compile-hdr (filter s-provide? headers))])
      #`(r:begin
         req ...
         #,(frame resugar
           #`(s-prog #,l (r:list #,@(map adorn headers)) __)
           (compile-pyret block resugar))
         prov ...))))

(define (compile-pyret ast resugar)
  (match ast
    [(s-prog l headers block)
     (compile-stepping-prelude (compile-prog l headers block resugar)
                               resugar)]
    [(s-block l stmts)
     (match-define (s-block l2 new-stmts) (lift-constants ast))
     (with-syntax ([(stmt ...) (compile-block l2 new-stmts (compile-env (set) #t) resugar)])
       (attach l #'(r:begin stmt ...)))]
    [else (error (format "Didn't match a case in compile-pyret: ~a" ast))]))

(define (compile-expr pre-ast resugar)
  (define ast (lift-constants pre-ast))
  (compile-expr/internal ast (compile-env (set) #f) resugar))

(define (discard-_ name)
  (if (equal? name '_) (gensym) name))

