#lang racket

(require "../pretty.rkt")

(provide (all-defined-out))

#| Tag ::= MacHead(macro-name, case-number, origin-term)
           -- Marks root of macro-originating code.
         | MacBody: Marks code that originated from a macro.
         | Alien: Marks code that is not from here. |#
(struct MacHead (m c q) #:transparent)
(struct MacBody () #:transparent)
(struct Alien () #:transparent)
  
#| ATerm ::= Node(Symbol, listof(ATerm))
           | Tagged(listof(Tag), ATerm)
           | Symbol
           | Integer
           | Float |#
(struct Node (label terms) #:transparent)
(struct List (terms) #:transparent)
(struct Tagged (tags term) #:transparent)
(struct CouldNotUnexpand ())

#| Term ::= TermList(listof(Tag), listof(Term))
          | TaggedTerm(listof(Tag), Term) |#
(struct TermList (tags terms) #:transparent)
(struct TermAtom (tags term) #:transparent)

  
; call/cc must be spelt call/cc, and not call-with-current-continuation

(define-struct Var (name value) #:transparent)
(define-struct Func (func term)
  #:property prop:procedure
  (λ (self . args) (apply (Func-func self) args)))
(define-struct Cont (stk))
(define undefined (letrec [[x x]] x))
(define (undefined? x) (eq? x undefined))

; define-setting : symbol symbol any
; Define a global mutable setting, e.g., for debugging
(define-syntax-rule (define-setting setting-name setter-id init-val)
  (begin
    (define setting-name init-val)
    (define (setter-id new-val)
      (set! setting-name new-val))))

(define-setting SHOW_PROC_NAMES     set-show-proc-names!     #t)
(define-setting SHOW_CONTINUATIONS  set-show-continuations!  #t)
(define-setting DEBUG_VARS          set-debug-vars!          #f)
(define-setting HIDE_EXTERNAL_CALLS set-hide-external-calls! #t)
(define-setting DEBUG_STEPS         set-debug-steps!         #f)
(define-setting HIDE_UNDEFINED      set-hide-undefined!      #t)
(define-setting DEBUG_COMMUNICATION set-debug-communication! #f)
(define-setting DEBUG_DESUGAR       set-debug-desugar!       #f)

(define expand (make-parameter #f))
(define unexpand (make-parameter #f))