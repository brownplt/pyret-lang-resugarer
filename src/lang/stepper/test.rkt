#lang racket

(require "../ast.rkt")
(require "../pretty.rkt")
(require "../get-syntax.rkt")
(require "../parser.rkt")
(require "data.rkt")
(require "resugar.rkt")

(set-debug-communication! #f)

(define (parse filename fileport)
  (s-prog-block (parse-program (get-syntax filename fileport))))

(define (test-desugar filename)
  (let [[ast (s-prog (empty-info 'test)
                     (list)
                     (parse filename (open-input-file filename)))]]
  (display (pretty ast))
  (display "\n-->\n")
  (display (pretty (resugarer:desugar "." "Prog" ast)))
  (display "\n\n")))

(set-debug-desugar! #f)

(test-desugar "tests/op.arr")
(test-desugar "tests/list.arr")
(test-desugar "tests/when.arr")
(test-desugar "tests/if.arr")
(test-desugar "tests/dot-colon-paren.arr")
(test-desugar "tests/for.arr")
(test-desugar "tests/left-app.arr")