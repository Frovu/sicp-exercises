#lang racket
(require racket/trace)

; ======================================================
; prime test and gcd
(define (square a) (* a a))
(define (divides? a b) (= (remainder b a) 0))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
		  ((divides? test-divisor n) test-divisor)
		  (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
	(= n (smallest-divisor n)))

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))
; ======================================================

(define (filtered-accumulate filter combiner null-value term a next b)
    (define (iter a result)
        (if (filter a)
            (if (>= a b)
                result
                (iter (next a) (combiner result (term a))))
            (iter (next a) result)))
    (trace iter)
    (iter a null-value))

(define (primes-sq-sum a b)
    (filtered-accumulate prime? + 0 square a add1 b))

(define (identity x) x)
(define (relp-product n)
    (filtered-accumulate
        (lambda (x) (= 1 (gcd x n)))
        * 1 identity 1 add1  n))

(primes-sq-sum 6 12) ; 49 + 121 = 170
(relp-product 10) ; 1 * 3 * 7 * 9 = 189
