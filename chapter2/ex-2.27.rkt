; Modify your reverse procedure of Exercise
; 2.18 to produce a deep-reverse procedure that takes a list
; as argument and returns as its value the list with its ele-
; ments reversed and with all sublists deep-reversed as well.
; For example,
; (define x (list (list 1 2) (list 3 4)))
; (reverse x)
; ((3 4) (1 2))
; (deep-reverse x)
; ((4 3) (2 1))
#lang racket
(require racket/trace)

(define (reverse l)
	(if (null? l) null
		(append (reverse (cdr l)) (list (car l)))))

(define (deep-reverse l)
	(if (null? l) null
		(append
			(deep-reverse (cdr l))
			(list (if (pair? (car l))
				(deep-reverse (car l))
				(car l))))))

; (trace deep-reverse)

(define x (list (list 1 2) (list 3 4)))
(reverse x)
(deep-reverse x)
