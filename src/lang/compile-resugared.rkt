#lang racket

(provide
  compile-pyret
  compile-expr)
(require
  racket/match
  racket/splicing
  racket/syntax
  "helpers.rkt"
  "../parameters.rkt"
  "ast.rkt"
  "pretty.rkt"
  "compile-helpers/find.rkt"
  "compile-helpers/lift-constants.rkt")


;;; Keeping track of the stack ;;;

(define (compile-stepping-prelude ast stx)
  (with-syntax [[code* stx]]
    #`(resugarer:with-resugaring
        (r:let []
          (r:define $emit (r:lambda (z)
                            (resugarer:emit z)))
          (resugarer:emit-first #,(adorn ast))
          code*))))

;;; Stepper ;;;

(define (frame fr expr)
  #`(r:with-continuation-mark
     (r:quote resugar-mark)
     (r:lambda (__) #,fr)
      (r:let [[result (r:let [] #,expr)]]
             ($emit result)
             result)))

(define (ephemeral-frame fr expr)
  #`(r:with-continuation-mark
     (r:quote resugar-mark)
     (r:lambda (__) #,fr)
      (r:let [[result (r:let [] #,expr)]]
             result)))

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
; Pyret-ast-with-values -> Racket-code-to-reconstruct-said-ast
(define (adorn x)
  (define (struct-name->constr struct-name)
    ; strip off "struct:"
    (let [[name (string->symbol (substring (symbol->string struct-name) 7))]]
      #`#,name))
  (cond [(or (string? x) (boolean? x) (number? x))
         #`#,x]
        [(symbol? x)
         #`(r:quote #,x)]
        [(list? x)
         (with-syntax [[(xs* ...) (map adorn x)]]
           #'(r:list xs* ...))]
;        [(s-id? x)
;         (with-syntax [[v* (s-id-id x)]]
;           #'(s:Var (r:quote v*) (r:quote v*)))] ; TODO(justin)
        [(srcloc? x)
         (match (reify-srcloc x)
           [(list src line col pos span)
            #`(s:srcloc #,src #,line #,col #,pos #,span (r:list #,@(srcloc-tags x)))])]
        [(struct? x)
         (let* [[children (vector->list (struct->vector x))]
                [constr (struct-name->constr (car children))]
                [subnodes (cdr children)]]
           #`(#,constr #,@(map adorn subnodes)))]
        [else
         (error (format "stepper/adorn: Unrecognized AST node type. ~a" x))]))



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

(define (compile-block l stmts env)
  (define (compile-stmt ast-node env add-frame)
    (match ast-node
      [(s-var s bind val)
        (list
          #`(r:define #,(discard-_ (s-bind-id bind))
                      #,(add-frame (frame #`(s-var #,s #,bind __)
                                          (compile-expr/internal val env))))
          #`p:nothing)]
      [(s-let s bind val)
       (define id (s-bind-id bind))
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
       (define (add-let-frame stx)
         (add-frame (frame #`(s-let #,s #,bind __) stx)))
       
       (match val
        [(s-lam l _ args _ doc body _)
         (define inline-binding
          (with-syntax ([(arg ...) (args-stx l args)])
            #`(r:define
               #,(make-immediate-id id)
               #,(add-let-frame
                  #`(p:arity-catcher (arg ...) #,(compile-expr/internal body env))))))
         (cond
          [(or (compile-env-toplevel? env) id-used)
            (list inline-binding
                  (with-syntax ([(arg ...) (args-stx l args)]
                                [f-id (make-immediate-id id)])
                    #`(r:define
                       #,(discard-_ id)
                       (p:pλ (arg ...) #,doc (f-id arg ...))))
                  #`nothing)]
          [else (list inline-binding)])]
        [(s-extend s (s-lam l _ args _ doc body _) fields)
         (define inline-binding
          (with-syntax ([(arg ...) (args-stx l args)])
            #`(r:define
               #,(make-immediate-id id)
               #,(add-let-frame
                  #`(p:arity-catcher (arg ...) #,(compile-expr/internal body env))))))
         (cond
          [(or (compile-env-toplevel? env) id-used)
            (list inline-binding
                  (with-syntax ([(arg ...) (args-stx l args)]
                                [f-id (make-immediate-id id)]
                                [(field ...) (map (curryr compile-member env) fields)])
                    #`(r:define
                       #,(discard-_ id)
                       #,(add-let-frame
                        #`(p:extend
                          #,(loc-stx s)
                          (p:pλ (arg ...) #,doc (f-id arg ...))
                          (r:list field ...)))))
                  #`nothing)]
           [else (list inline-binding #`nothing)])]
        [_ (list #`(r:define
                    #,(discard-_ id)
                    #,(add-let-frame (compile-expr/internal val env)))
                 #`nothing)])]
      [_ (list (add-frame (compile-expr/internal ast-node env)))]))
  (define (compile-stmts stmts env)
    (if (empty? stmts)
        (list)
        (let* [[fr #`(s-block
                      #,l (r:list __ #,@(map adorn (cdr stmts))))]
               [add-frame (λ (stx)
                 (frame fr stx))]]
          (append (compile-stmt (car stmts) env add-frame)
                  (compile-stmts (cdr stmts) env)))))
  (define ids (block-ids stmts))
  (define fun-ids (block-fun-ids stmts))
  (define old-fun-ids (compile-env-functions-to-inline env))
  (define avoid-shadowing (set-subtract old-fun-ids (list->set ids)))
  (define new-env (compile-env (set-union avoid-shadowing fun-ids)
                               (compile-env-toplevel? env)))
  (define stmts-stx (compile-stmts stmts new-env))
  (if (empty? stmts-stx)
      (list #'nothing) ;!!!
      stmts-stx))

(define (compile-member ast-node env)
  (match ast-node
    [(s-data-field l name value)
     (attach l
       (with-syntax*
        ([name-stx (compile-string-literal l name env)]
         [val-stx (compile-expr/internal value env)])
         #`(r:cons name-stx val-stx)))]))
(define (compile-string-literal l e env)
  (match e
    [(s-str _ s) (d->stx s l)]
    [else #`(p:check-str #,(compile-expr/internal e env) #,(loc-stx l))]))

(define (compile-expr/internal ast-node env)
  (define compile-expr compile-expr/internal)
  (define (mark l expr)
    (with-syntax [((loc-param ...) (loc-list l))]
      #`(r:with-continuation-mark (r:quote pyret-mark) (srcloc loc-param ... (r:list)) #,expr)))
  (define (mark-if mark-mode l expr)
    (if mark-mode (mark l expr) expr))
  (define (compile-body l body new-env)
    (mark l (compile-expr body new-env)))
  (define (compile-lookup l obj field lookup-type bracket-constr)
     (attach l
      (with-syntax*
         ([field-stx (compile-string-literal l field env)]
          [temp (gensym 'lkup)])
         #`(r:let [[temp #,(frame #`(#,bracket-constr #,l __ #,(adorn field))
                                  (compile-expr obj env))]]
             (#,lookup-type #,(loc-stx l) temp
                            #,(frame #`(#,bracket-constr #,l temp __)
                                     (compile-string-literal l field env)))))))

  (define (compile-args add-frame fvar used-vars unused-vars unused-args unused-args-stx env)
    (if (empty? unused-args)
        #`(#,fvar #,@used-vars)
        #`(r:let [[#,(car unused-vars)
                   #,(add-frame (append used-vars (list '__) (map adorn (cdr unused-args)))
                                (car unused-args-stx))]]
                 #,(compile-args add-frame fvar
                                 (append used-vars (list (car unused-vars)))
                                 (cdr unused-vars)
                                 (cdr unused-args)
                                 (cdr unused-args-stx)
                                 env))))
  
  (define (compile-app l fun-stx hidden-args args-stx args env)
    (let [[vars (map (lambda (_) (gensym 'arg)) args)]
          [fvar (gensym 'fun)]]
      (define (add-frame args body) (frame #`(s-app #,l #,fvar (r:list #,@args)) body))
      #`(r:let [[#,fvar #,(ephemeral-frame #`(s-app #,l __ (r:list #,@(map adorn args)))
                                 fun-stx)]]
               #,(compile-args add-frame fvar hidden-args vars args args-stx env))))

  
  (match ast-node

    [(s-block l stmts)
     (define new-env (compile-env (compile-env-functions-to-inline env) #f))
     (with-syntax ([(stmt ...) (compile-block l stmts new-env)])
       (attach l #'(r:let () stmt ...)))]

    [(s-user-block l body) (compile-expr body env)]

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
     (attach l
       (match c-bs
         [(list)
          (compile-expr else-block env)]
         [(cons (s-if-branch s test block) brs)
          #`(r:if
             (p:pyret-true?
              #,(frame
                 #`(s-if-else
                    #,l
                    (r:cons (s-if-branch #,s __ #,(adorn block))
                            (r:list #,@(map adorn brs)))
                    #,(adorn else-block))
                 (compile-expr test env)))
             #,(compile-expr block env)
             #,(compile-expr (s-if-else l brs else-block) env))]))]

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
         #`(r:let [(temp
                    #,(frame #`(s-assign #,l (r:quote #,name) __)
                             (compile-expr expr env)))]
                  (r:set! name-stx temp)
                  temp)))]

    [(s-app l fun args)
     (define compiled-fun
       (match fun
         [(s-id l2 (? (λ (s) (set-member? (compile-env-functions-to-inline env) s)) id))
          (make-immediate-id id)]
         [(s-lam l _ args _ doc body _)
          (with-syntax ([(arg ...) (args-stx l args)])
            #`(p:arity-catcher (arg ...) #,(compile-expr/internal body env)))]
         [_ #`(p:p-base-app #,(compile-expr fun env))]))
     (define compiled-args
       (map (lambda (arg) (compile-expr/internal arg env)) args))
     (mark-if (current-mark-mode) l
       (attach l
         (compile-app l compiled-fun (list) compiled-args args env)))]

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

    [(s-update l super fields)
     (attach l
       (with-syntax ([(member ...) (map (curryr compile-member env) fields)]
                     [super (compile-expr super env)])
        #`(p:update #,(loc-stx l)
                    super
                    (r:list member ...))))]

    [(s-bracket l obj field)
     (compile-lookup l obj field #'p:get-field s-bracket)]

    [(s-get-bang l obj field)
     (attach l
      #`(p:get-mutable-field #,(loc-stx l) #,(compile-expr obj env) #,(symbol->string field)))]

    [(s-colon-bracket l obj field)
     (compile-lookup l obj field #'p:get-raw-field s-colon-bracket)]

    [(s-prog l headers block) (compile-prog l headers block)]

    [else (error (format "Missed a case in compile: ~a" ast-node))]))

(define (compile-header header)
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
            (r:define temp-stx #,(compile-expr exp))
            (r:provide (r:rename-out [temp-stx %PYRET-PROVIDE])))))]))



(define (compile-prog l headers block)
  (attach l
   (with-syntax ([(req ...) (map compile-header (filter s-import? headers))]
                 [(prov ...) (map compile-header (filter s-provide? headers))])
     (ephemeral-frame #`(s-prog #,l (r:list #,@(map adorn headers)) __)
        #`(r:begin req ... #,(compile-pyret block) prov ...)))))

(define (maybe-lift-constants ast) ast)
;  (cond
;    [(current-compile-lift-constants) (lift-constants ast)]
;    [else ast]))

(define (compile-pyret ast)
  (match ast
    [(s-prog l headers block)
     (compile-stepping-prelude (s-prog l headers block)
                               (compile-prog l headers block))]
    [(s-block l stmts)
     (match-define (s-block l2 new-stmts) (maybe-lift-constants ast))
     (with-syntax ([(stmt ...) (compile-block l2 new-stmts (compile-env (set) #t))])
       (attach l #'(r:begin stmt ...)))]
    [else (error (format "Didn't match a case in compile-pyret: ~a" ast))]))

(define (compile-expr pre-ast)
  (define ast (maybe-lift-constants pre-ast))
  (compile-expr/internal ast (compile-env (set) #f)))

(define (discard-_ name)
  (if (equal? name '_) (gensym) name))