; Exercise 2.1: Define a better version of make-rat that han-
; dles both positive and negative arguments. make-rat should
; normalize the sign so that if the rational number is positive,
; both the numerator and denominator are positive, and if
; the rational number is negative, only the numerator is negative.
#lang racket

(define (add-rat x y)
	(make-rat (+
			(* (numer x) (denom y))
			(* (numer y) (denom x)))
		(* (denom x) (denom y))))

(define (sub-rat x y)
	(make-rat (- (* (numer x) (denom y))
		(* (numer y) (denom x)))
		(* (denom x) (denom y))))

(define (mul-rat x y)
	(make-rat (* (numer x) (numer y))
		(* (denom x) (denom y))))

(define (div-rat x y)
	(make-rat (* (numer x) (denom y))
		(* (denom x) (numer y))))

(define (equal-rat? x y)
	(= (* (numer x) (denom y))
		(* (numer y) (denom x))))

(define (print-rat x)
	(display (numer x))
	(display "/")
	(display (denom x)))
	(newline)

(define (numer x) (car x))
(define (denom x) (cdr x))
; (define (make-rat n d)
; 	(let ((g (gcd n d)))
; 		(cons (/ n g) (/ d g))))

(define (make-rat n d)
	(let ([g (gcd n d)])
		(let ([num (/ n g)] [den (/ d g)])
			(cons
				(if (< den 0) (* -1 num) num)
				(if (< den 0) (* -1 den) den)))))

(define one-half (make-rat -1 2))
(define one-third (make-rat -1 -3))
(print-rat (add-rat one-half one-third))
