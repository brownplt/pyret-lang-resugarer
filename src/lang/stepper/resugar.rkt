#lang racket

(require "data.rkt")
(require "convert-ast.rkt")
(require "../ast.rkt")
(require "../pretty.rkt")
; debugging:
(require "../desugar.rkt")

(set-debug-desugar! #f)
(set-debug-communication! #t)

(provide (rename-out
          [with-resugaring resugarer:with-resugaring]
          [desugar resugarer:desugar]
          [emit resugarer:emit]
          [reconstruct-stack resugarer-test:reconstruct-stack])
         (prefix-out s:
           (combine-out
            (struct-out srcloc)
            (struct-out info)
            (struct-out Val)
            (struct-out Var)
            (struct-out Func)
            (struct-out Cont))))

(define (send-command cmd out)
  (when DEBUG_COMMUNICATION (display cmd) (newline))
  (display cmd out)
  (flush-output out))
  
(define (read-port port [str ""])
  (let [[line (read-line port)]]
    (if (eof-object? line)
        str
        (read-port port (string-append str line)))))
  
(define (receive-response in err)
  (let [[response (read-line in)]]
    (when DEBUG_COMMUNICATION (display response) (newline) (newline))
    (cond [(eof-object? response)
           (display (read-port err)) (newline)
           (error "Received EOF")]
          [(strip-prefix "success: " response)
           => (λ (t) (string->ast t))]
          [(strip-prefix "failure: " response)
           => (λ (_) (CouldNotUnexpand))]
          [(strip-prefix "error: " response)
           => (λ (msg) (error msg))])))

(define-syntax-rule (with-resugaring dir expr exprs ...)
  (begin
    (current-locale "en_US.UTF-8") ; How to make Racket read in unicode?
    (let [[cmd-file (string-append dir "/Resugarer")]
          [grammar-file (string-append dir "/pyret.grammar")]]
    (let-values [[(resugarer in out err)
                  (subprocess #f #f #f cmd-file grammar-file)]]
      (parameterize
          [[expand (λ (t sort)
             (send-command (format "desugar ~a ~a\n" sort (ast->string t)) out)
             (if DEBUG_DESUGAR
                 (let [[response (receive-response in err)]]
                   (display (format "desugaring...\n  --std-->\n~a\n f --new-->\n~a\n\n" (pretty (desugar-pyret t)) (pretty response)))
                   response)
             (receive-response in err)))]
           [unexpand (λ (t sort [keep-srclocs? #t])
             (send-command (format "resugar ~a ~a\n" sort (ast->string t keep-srclocs?)) out)
             (receive-response in err))]]
        (let [[result (begin expr exprs ...)]]
          (subprocess-kill resugarer #t)
          result))))))

(define (desugar dir sort ast)
  (with-resugaring dir ((expand) ast sort)))

(define (reconstruct-stack x)
  (let [[stk (continuation-mark-set->list
              (current-continuation-marks)
              'resugar-mark)]]
    (define (rec x stk)
      (if (empty? stk)
          x
          (rec ((car stk) x) (cdr stk))))
    (rec x stk)))

(define (display-skip t)
  (when DEBUG_STEPS
    (display (format "SKIP: ~a\n\n" (pretty t)))))

(define (display-step t)
  (display (format "\n~a\n" (pretty t)))
  (when DEBUG_STEPS (newline)))
    

(define (sort t)
  (cond [(s-prog? t) "Prog"]
        [(s-block? t) "Block"]
        [else "Expr"]))

(define (emit x [id #f])
  (display (format "EMIT: ~a\n\n" (reconstruct-stack x)))
  (if id
      (let* [[name (Var-name x)]
             [t (Var-value x)]
             [u ((unexpand) t (sort t) #f)]]
        (if (CouldNotUnexpand? u) (emit x) (void)))
      (let* [[t (reconstruct-stack x)]
             [u ((unexpand) t (sort t) #f)]]
        #;(display-step t)
        (if (CouldNotUnexpand? u)
            (display-skip t)
            (display-step u)))))

(define (value->term x)
  (cond [(Func? x)
         (value->term (Func-term x))]
        [(TermList? x)
         (TermList (TermList-tags x) (map value->term (TermList-terms x)))]
        [(Var? x)
         (let* [[name (Var-name x)]
                [term (value->term (Var-value x))]
                [u    ((unexpand) term "Prog")]]
           (if DEBUG_VARS
               (TermList (list) (list name ':= term))
               (if (or (and HIDE_UNDEFINED (undefined? u))
                       (CouldNotUnexpand? u))
                   name u)))]
        [(Cont? x)
         (let [[stk (value->term (reconstruct-stack '__ (Cont-stk x)))]]
           (TermList (list) (list '*cont* stk)))]
        [(and SHOW_PROC_NAMES (procedure? x))
         (or (object-name x) 'cont)]
        [else
         x]))


;;; Helpers ;;;

; string-prefix? : string -> string -> bool
; Does 'string' begin with 'prefix'?
(define (string-prefix? prefix string)
  (and (>= (string-length string) (string-length prefix))
       (string=? prefix (substring string 0 (string-length prefix)))))

; strip-prefix : string -> string -> string | #f
; Strip 'prefix' off of the front of 'string', else return #f
(define (strip-prefix prefix string)
  (if (string-prefix? prefix string)
      (substring string (string-length prefix))
      #f))