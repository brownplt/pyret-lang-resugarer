#lang racket

(require "../ast.rkt")
(require "../pretty.rkt")
(require "../get-syntax.rkt")
(require "../parser.rkt")
(require "data.rkt")
(require "resugar.rkt")
(require racket/runtime-path)

(set-debug-communication! #f)

(define (parse filename fileport)
  (s-prog-block (parse-program (get-syntax filename fileport))))

(define (test-desugar filename)
  (let [[ast (s-prog (empty-srcloc 'test)
                     (list)
                     (parse filename (open-input-file filename)))]]
    (display (pretty ast))
    (display "\n-->\n")
    (display (pretty (resugarer:desugar "." "Prog" ast)))
    (display "\n\n")))
  
(set-debug-desugar! #f)
(set-debug-communication! #f)

(test-desugar "tests/op.arr")
(test-desugar "tests/list.arr")
(test-desugar "tests/when.arr")
(test-desugar "tests/if.arr")
(test-desugar "tests/dot-colon-paren.arr")
(test-desugar "tests/for.arr")
(test-desugar "tests/left-app.arr")

(define (frame fr expr)
  #`(with-continuation-mark 'resugar-mark
      (lambda (__) #,fr)
      (let [[result #,expr]]
        (display (resugarer-test:reconstruct-stack result))
        (newline)
        result)))

(define (compile x)
  (match x
    [`(if ,x ,y ,z)
     #`(if (eq? 0 #,(frame #`(list 'if __ '** '**) (compile x)))
           #,(compile y)
           #,(compile z))]
    [`(+ ,x ,y)
     #`(+ #,(frame #`(list '+ __ '**) (compile x))
          #,(frame #`(list '+ '** __) (compile y)))]
    [x #`#,x]))

(define ns (current-namespace))

(eval (compile '(if (+ 1 2) (+ 3 4) (+ 5 6))) ns)
            