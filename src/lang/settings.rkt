#lang racket/base

(provide param-compile-check-mode
         param-compile-resugar-mode)

(define param-compile-check-mode (make-parameter #f))

(define param-compile-resugar-mode (make-parameter #f))