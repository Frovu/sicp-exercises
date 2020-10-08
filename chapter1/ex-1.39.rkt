#lang racket
(require racket/trace)

(define (cont-frac n d k)
    (define (iter i result)
        (if (= i 0)
            result
            (iter (sub1 i) (/ (n i) (+ result (d i))))))
    (trace iter)
    (iter k 0))

(define (tan-cf x k)
    (/ x (+ 1 (cont-frac
        (lambda (i) (* -1 x x))
        (lambda (i) (add1 (* 2 i)))
        k))))

(tan-cf 10.0 30)
(tan 10.0)
