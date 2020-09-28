#lang racket
(require racket/trace)

; This algorithm takes a number of steps that is linear in b
(define (mul a b)
	(if (= b 0)
		0
		(+ a (mul a (- b 1)))))

;  Now suppose we include, together with addition, opera-
; tions double , which doubles an integer, and halve , which
; divides an (even) integer by 2. Using these, design a mul-
; tiplication procedure analogous to fast-expt that uses a
; logarithmic number of steps.

(define (double a) (* a 2))
(define (halve  a) (/ a 2))

(define (mul-it a b)
	(cond ((= 0 b) 0)
		  ((even? b) (mul-it (double a) (halve b)))
		  (else (+ a (mul-it a (sub1 b))))))

; tests
(trace mul)
(mul 13 21)
(trace mul-it)
(mul-it 13 21)
