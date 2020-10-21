#lang racket

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f times)
  (define (rep-it result n)
    (if (= n 0) result
      (rep-it (compose result f) (sub1 n))))
  (rep-it f (sub1 times)))

(define (square a) (* a a))
((repeated square 2) 2)
((repeated square 2) 5)
((repeated square 1) 10)
