; The value returned by the call to for-each (not illustrated
; above) can be something arbitrary, such as true. Give an
; implementation of for-each.
#lang racket

(define (for-each proc li)
	(if (null? li) #t
		(begin
			(proc (car li))
			(for-each proc (cdr li)))))

(for-each
	(lambda (x)
		(display x)
		(newline))
	(list 57 321 88))
