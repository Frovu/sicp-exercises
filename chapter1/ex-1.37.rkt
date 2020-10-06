#lang racket
(require racket/trace)

;  Define a procedure cont-frac such that evaluating
; (cont-frac n d k) computes the value of the k-term
; finite continued fraction.

(define (cont-frac n d k)
    (define (iter i result)
        (if (= i 0)
            result
            (iter (sub1 i) (/ (n i) (+ result (d i))))))
    (trace iter)
    (iter k 0))

(define (cont-frac-rec n d k)
    (define (rec i)
        (if (= i k)
            (/ (n i) (d i))
            ( / (n i) (+ (d i) (rec (add1 i))))))
    (trace rec)
    (rec 1))

(/ 1
  (cont-frac-rec
    (lambda (i) 1.0)
    (lambda (i) 1.0)
    100))
; = 1.618033988749895
