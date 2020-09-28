#lang racket

(define (triangle col row)
	(if (or (< col 2) (>= col row))
		1
		(+ (triangle (sub1 col) (sub1 row)) (triangle col (sub1 row)))))

(define (triangle-row r)
	(map (lambda (y) (triangle y r)) (range 1 (add1 r))))

(triangle-row 1)
(triangle-row 2)
(triangle-row 3)
(triangle-row 4)
(triangle-row 5)
(triangle-row 6)
(triangle-row 7)
(triangle-row 8)