#lang racket

(define (square a) (* a a))

(define (iterative-improve good-enough? improve)
  (define (iter guess) (if (good-enough? guess) guess (iter (improve guess))))
  iter)

; square roots
(define (average x y)
  (/ (+ x y) 2))
(define (sqrt x)
  ((iterative-improve
    (lambda (guess) (< (abs (- (square guess) x)) 0.001))
    (lambda (guess) (average guess (/ x guess))))
   x))

(sqrt 2.0)
(sqrt 121.0)

; fixed points

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  ((iterative-improve
    (lambda (guess) (< (abs (- guess (f guess))) tolerance))
    (lambda (guess) (f guess)))
   first-guess))

(fixed-point cos 1.0)
