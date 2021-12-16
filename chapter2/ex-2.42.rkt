#lang racket
(require racket/trace)

(define (flatmap proc seq)
	(foldr append null (map proc seq)))

(define empty-board null)
(define (adjoin-position row k board) (append board (list (list row k))))

(define (queens board-size)
	(define (queen-cols k)
		(if (= k 0)
			(list empty-board)
			(filter
				(lambda (positions) (safe? positions))
				(flatmap
					(lambda (rest-of-queens)
						(map (lambda (new-row)
							(adjoin-position new-row k rest-of-queens))
							(inclusive-range 1 board-size)))
					(queen-cols (- k 1))))))
	(trace queen-cols)
	(queen-cols board-size))

(define (safe? positions)
	(if (= 1 (length positions)) #t
		(let ([pos (car positions)] [kth (last positions)])
			(and
				(and
					(not (= (car pos) (car kth)))
					(not (= (cadr pos) (cadr kth)))
					(not (= (- (car pos) (car kth)) (- (cadr pos) (cadr kth)))))
				(safe? (cdr positions))))))

; (trace queens)
; (trace adjoin-position)
; (trace safe?)

(for-each (lambda (a) (newline) (display a) ) (queens 3))
(newline)
