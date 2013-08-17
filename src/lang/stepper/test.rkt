#lang racket

(require "../ast.rkt")
(require "../pretty.rkt")
(require "../get-syntax.rkt")
(require "../parser.rkt")
(require "data.rkt")
(require "desugar.rkt")

(set-debug-communication! #t) 

(define (parse filename fileport)
  (s-prog-block (parse-program (get-syntax filename fileport))))

(define (test-desugar filename)
  (display (desugar (parse filename (open-input-file filename))))
  ;(display (pretty (desugar (parse filename (open-input-file filename)))))
  (newline) (newline))

(test-desugar "tests/ahoy-world.arr")
(test-desugar "tests/if.arr")