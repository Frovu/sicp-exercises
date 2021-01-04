; Write a procedure fringe that takes as argument a tree
; (represented as a list) and returns a list whose elements are
; all the leaves of the tree arranged in left-to-right order.
#lang racket
(require racket/trace)

(define (fringe tree)
	(cond [(null? tree) null]
		[(not (pair? tree)) (list tree)]
		[else (append (fringe (car tree)) (fringe (cdr tree)))]))

(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))
