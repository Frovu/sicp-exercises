#lang racket
(require racket/trace)

(define (accumulate op initial sequence)
	(if (null? sequence)
			initial
			(op (car sequence)
					(accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
	(if (null? (car seqs))
		null
		(cons (accumulate op init (map car seqs))
			(accumulate-n op init (map cdr seqs)))))

(define (show mat)
	(for-each (lambda (l) (begin (display l) (newline))) mat))

(define (transpose mat)
	(accumulate-n cons '() mat))

(define (dot-product v w)
	(accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
	(map (lambda (l) (dot-product v l)) m))

(define (matrix-*-matrix m n)
	(let ((cols (transpose n)))
		(map (lambda (line) (matrix-*-vector cols line)) m)))


(define ma (list '(1 2 3) '(4 5 6)))
(define mb (list '(1 2) '(3 4) '(5 6)))

(show ma)
(newline)
(dot-product '(1 2 3) '(4 5 6))
(newline)
(show (matrix-*-matrix ma mb))
