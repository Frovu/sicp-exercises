#lang racket
(require racket/trace)

(define (square a) (* a a))
(define (expmod base exp m)
	(cond ((= exp 0) 1)
		  ((even? exp)
		   (remainder
				(square (expmod base (/ exp 2) m))
			 	m))
		  (else
			  (remainder 
				(* base (expmod base (- exp 1) m))
				m))))

(define (test-fermat-test n)
	(define (test-it a)
		(cond ((= a 0) true)
			  ((= (expmod a n n) a) (test-it (sub1 a)))
			  (else false)))
	(test-it (sub1 n)))

(define carmichael-numbers '(561 1105 1729 2465 2821 6601))
(map test-fermat-test carmichael-numbers)
; all numbers pass as prime, so they fool Fermat test