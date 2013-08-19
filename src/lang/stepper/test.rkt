#lang racket

(require "../ast.rkt")
(require "../pretty.rkt")
(require "../get-syntax.rkt")
(require "../parser.rkt")
(require "data.rkt")
(require "runtime.rkt")

(set-debug-communication! #f) 

(define (parse filename fileport)
  (s-prog-block (parse-program (get-syntax filename fileport))))

(define (test-desugar filename)
  (display (pretty (parse filename (open-input-file filename))))
  (newline) (display "-->") (newline)
  (display (pretty (resugarer:desugar "." "Block"
                    (parse filename (open-input-file filename)))))
  (newline) (newline))

(test-desugar "tests/ahoy-world.arr")
(test-desugar "tests/if.arr")