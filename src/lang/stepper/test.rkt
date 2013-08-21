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

(test-desugar "tests/op.arr")
(test-desugar "tests/list.arr")
(test-desugar "tests/when.arr")
(test-desugar "tests/if.arr")
(test-desugar "tests/dot-colon-paren.arr")
(test-desugar "tests/for.arr")
(test-desugar "tests/left-app.arr")