#lang racket
(require racket/trace)

(define (subsets s)
	(if (null? s)
		(list null)
		(let ((rest (subsets (cdr s))))
			(append rest (map (lambda (c) (cons (car s) c)) rest)))))

(subsets '(1 2 3 4 5))
