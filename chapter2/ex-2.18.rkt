#lang racket

(define (reverse l)
	(if (null? l) null
		(append (reverse (cdr l)) (list (car l)))))

(define (reverse-it l)
	(define (iter res rem)
		(if (null? rem) res
			(iter (cons (car rem) res) (cdr rem))))
	(iter '() l))

(reverse (list 1 4 9 16 25))
(reverse-it (list 1 4 9 16 25))
