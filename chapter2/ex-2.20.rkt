; write a procedure same-parity that
; takes one or more integers and returns a list of all the ar-
; guments that have the same even-odd parity as the first
; argument. For example,
; (same-parity 1 2 3 4 5 6 7)
; (1 3 5 7)
; (same-parity 2 3 4 5 6 7)
; (2 4 6)
#lang racket

(define (same-parity a . rest)
	(define (same-p rem)
		(if (null? rem) rem
			(if (equal? (odd? a) (odd? (car rem)))
				(cons (car rem) (same-p (cdr rem)))
				(same-p (cdr rem)))))
	(cons a (same-p rest)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
