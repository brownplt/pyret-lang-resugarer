#lang racket

(require "data.rkt")
(require "convert-ast.rkt")
(require "../ast.rkt")

(provide with-resugaring desugar)

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

(define-syntax-rule (with-resugaring expr ...)
  (begin
    (current-locale "en_US.UTF-8") ; How to make Racket read in unicode?
    (let-values [[(resugarer in out err)
                  (subprocess #f #f #f "Resugarer" "pyret.grammar")]]
      (parameterize
          [[expand (λ (t)
             (send-command (format "desugar Block ~a\n" (ast->string t)) out)
             (receive-response in err))]
           [unexpand (λ (t)
             (send-command (format "resugar Block ~a\n" (ast->string t)) out)
             (receive-response in err))]]
        (let [[result (begin expr ...)]]
          (subprocess-kill resugarer #t)
          result)))))

(define (desugar ast)
  (with-resugaring ((expand) ast)))


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