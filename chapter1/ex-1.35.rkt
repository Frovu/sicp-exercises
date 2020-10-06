#lang racket
(require racket/trace)

;  Show that the golden ratio φ (Section 1.2.2) is a fixed
; point of the transformation x 7→ 1 + 1/x , and use this
; fact to compute φ by means of the fixed-point procedure.

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

(fixed-point (lambda (x) (add1 (/ 1.0 x))) 1.0)
; = 1.6180327868852458 (golden ratio)
