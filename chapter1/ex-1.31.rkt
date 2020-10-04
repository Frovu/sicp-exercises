#lang racket
(require racket/trace)

(define (product-it term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* result (term a)))))
    (iter a 1))

(define (product-rec term a next b)
    (if (> a b)
        1
        (* (term a)
            (product-rec term (next a) next b))))

(define (identity x) x)
(define (factorial n)
    (product-it identity 1 add1 n))

(define (pi-term a)
    (/
        (if (even? a) a (sub1 a))
        (if (even? a) (sub1 a) a)))

(define (pi-approx n)
    (* 4 (product-it pi-term 3.0 add1 n)))

(factorial 5)
(pi-approx 1000) ; 3.143163842419187
