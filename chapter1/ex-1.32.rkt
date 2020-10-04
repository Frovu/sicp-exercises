#lang racket
(require racket/trace)

(define (accumulate combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner result (term a)))))
    (iter a null-value))

(define (accumulate-rec combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
            (accumulate-rec term (next a) next b))))

(define (sum term a next b) (accumulate + 0 term a next b))
(define (product term a next b) (accumulate * 1 term a next b))

(define (identity x) x)
(define (factorial n)
    (product identity 1 add1 n))

(define (pi-term a)
    (/
        (if (even? a) a (sub1 a))
        (if (even? a) (sub1 a) a)))

(define (pi-approx n)
    (* 4 (product pi-term 3.0 add1 n)))

(define (pi-sum a b)
    (define (pi-term x)
        (/ 1.0 (* x (+ x 2))))
    (define (pi-next x)
        (+ x 4))
    (sum pi-term a pi-next b))

(factorial 5)
(* 8 (pi-sum 1 10000)) ; 3.141392653591789
(pi-approx 10000) ; 3.1417497371492233
