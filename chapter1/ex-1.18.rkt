#lang racket
(require racket/trace)

; This algorithm takes a number of steps that is linear in b
(define (mul a b)
	(if (= b 0)
		0
		(+ a (mul a (- b 1)))))

; Fast recursive algorithm

(define (double a) (* a 2))
(define (halve  a) (/ a 2))

(define (mul-fast a b)
	(cond ((= 0 b) 0)
		  ((even? b) (mul-fast (double a) (halve b)))
		  (else (+ a (mul-fast a (sub1 b))))))

; Devise a procedure that generates an iterative process
; for multiplying two integers in terms of adding,
; doubling, and halving and uses a logarithmic number of steps.

(define (mul-it a b) (mul-it-iter a b 0))
(define (mul-it-iter a b sum)
(cond ((= 0 b) sum)
	  ((even? b) (mul-it-iter (double a) (halve b) sum))
	  (else (mul-it-iter a (sub1 b) (+ sum a)))))
	
; tests
;(trace mul)
;(mul 13 21)
(trace mul-fast)
(mul-fast 13 99)
(trace mul-it-iter)
(mul-it 13 99)
