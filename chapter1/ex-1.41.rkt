#lang racket
(require racket/trace)

(define (double proc)
  (lambda (x) (proc (proc x))))

((double add1) 3)
(((double (double double)) add1) 10)
