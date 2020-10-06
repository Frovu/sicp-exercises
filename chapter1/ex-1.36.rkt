#lang racket
(require racket/trace)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2))
            tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (trace try)
    (try first-guess))

(define (average a b) (/ (+ a b) 2))
(define (approx x)
    (fixed-point
        (lambda (y) (average y (/ (log 1000) (log y))))
        2.0))

(approx 1000)
; without dumping: 4.555532270803653 in 33 steps
; with dumping: 4.555537551999825 in 8 steps
