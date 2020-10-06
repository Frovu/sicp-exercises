#lang racket
(require racket/trace)

(define (cont-frac n d k)
    (define (iter i result)
        (if (= i 0)
            result
            (iter (sub1 i) (/ (n i) (+ result (d i))))))
    (trace iter)
    (iter k 0))

(define (approx-e k)
    (+ 2 (cont-frac
        (lambda (i) 1.0)
        (lambda (i)
            (if (= 0 (remainder (add1 i) 3))
                (* 2 (/ (add1 i) 3))
                1))
        k)))

(approx-e 100)
; = 2.7182818284590455
