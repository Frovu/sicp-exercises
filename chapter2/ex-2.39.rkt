#lang racket

(define (reverse sequence)
	(foldl cons null sequence))

(reverse '(1 2 3 5))
