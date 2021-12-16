#lang racket
(require sicp-pict)

(define (paint-to-png painter filename)
	(define snip   (paint painter))
	(define bitmap (send snip get-bitmap))
	(send bitmap save-file filename 'png))

(define (right-split painter n)
	(if (= n 0)
		painter
		(let ((smaller (right-split painter (- n 1))))
			(beside painter (below smaller smaller)))))

(define (up-split painter n)
	(if (= n 0)
		painter
		(let ((smaller (up-split painter (- n 1))))
			(below painter (beside smaller smaller)))))

(define (corner-split painter n)
	(if (= n 0)
		painter
		(let ((up (up-split painter (- n 1)))
				(right (right-split painter (- n 1))))
			(let ((top-left (beside up up))
					(bottom-right (below right right))
					(corner (corner-split painter (- n 1))))
				(beside (below painter top-left)
					(below bottom-right corner))))))

(paint-to-png (corner-split einstein 4) "png/2.44.png")
