#lang racket
(require racket/trace)

(define (make-mobile left right)
	(list left right))
(define (make-branch length structure)
	(list length structure))

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

(define (branch-weight branch)
	(let ([bs (branch-structure branch)])
		(if (pair? bs) (total-weigth bs) bs)))
(define (total-weigth mobile)
	(+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

(define (torque branch)
	(* (branch-weight branch) (branch-length branch)))
(define (balanced? mobile)
	(and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
		(let ([lbs (branch-structure (left-branch mobile))]
					[rbs (branch-structure (right-branch mobile))])
			(and (or (not (pair? lbs)) (balanced? lbs))
					(or (not (pair? rbs)) (balanced? rbs))))))

(define a (make-branch 8 3))
(define b (make-branch 4 6))
(define ab (make-branch 2 (make-mobile a b)))
(define mob (make-mobile (make-branch 3 6) ab))

(total-weigth mob)
(balanced? mob)
