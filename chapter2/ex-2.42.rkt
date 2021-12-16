#lang racket
(require racket/trace)

(define (flatmap proc seq)
	(foldr append null (map proc seq)))

(define empty-board null)

; it would be more efficient to store only rows since cols are always consecutive
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
	(queen-cols board-size))

(define (safe? positions)
	(if (= 1 (length positions)) #t
		(let ([row (caar positions)] [k-row (car (last positions))]
				[col (cadar positions)] [k-col (cadr (last positions))])
			(and
				(and (not (= row k-row)) ; row (k-col > col always)
					(not (= (- k-col col) (abs (- k-row row))))) ; diagonal
				(safe? (cdr positions))))))

; (trace queens)
; (trace adjoin-position)
; (trace safe?)

(for-each (lambda (a) (newline) (display a) ) (queens 4))
(newline)
(newline)
(length (queens 12)) ; takes about 10 s
