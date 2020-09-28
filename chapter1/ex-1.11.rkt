#lang racket

(define (f-rec n)
	(if (< n 3) n (+ 
			(f-rec (- n 1))
			(* 2 (f-rec (- n 2)))
			(* 3 (f-rec (- n 3))))))

(define (f-it-iter a b c count)
	(if (<= count 0)
		c
		(f-it-iter b c (+ (* 3 a) (* 2 b) c) (- count 1))))

(define (f-it n)
	(if (< n 3)
		n 
		(f-it-iter 0 1 2 (- n 2))))

(map f-rec (range 10))
(map f-it (range 10))