#lang racket

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment strt end) (cons strt end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (midpoint-segment s)
	(let ([strt (start-segment s)] [end (end-segment s)])
		(make-point
			(/ (+ (x-point strt) (x-point end)) 2)
			(/ (+ (y-point strt) (y-point end)) 2))))

(define (print-point p)
	(display "(")
	(display (x-point p))
	(display ", ")
	(display (y-point p))
	(display ")"))
	(newline)

(define a (make-point 1 1))
(define b (make-point 9 5))
(define s (make-segment a b))

(print-point (midpoint-segment s))
