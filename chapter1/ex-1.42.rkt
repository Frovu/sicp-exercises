#lang racket
(require racket/trace)

(define (compose f g)
  (lambda (x) (f (g x))))

((compose sub1 add1) 3)
((compose (lambda (x) (* x x)) add1) 10)
