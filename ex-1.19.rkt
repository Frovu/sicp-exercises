#lang racket
(require racket/trace)
; There is a clever algorithm for computing
; the Fibonacci numbers in a logarithmic number of steps.
;  . . . Put this all together to complete the following
; procedure, which runs in a logarithmic number of steps

(define (fib n)
    (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
    (cond
        ((= count 0) b)
        ((even? count)
            (fib-iter a
                b
                (+ (* q q) (* p p))
                (+ (* q q) (* 2 p q))
                (/ count 2)))
        (else (fib-iter
            (+ (* b q) (* a q) (* a p))
            (+ (* b p) (* a q))
            p
            q
            (- count 1)))))

;T(a,b) = (bq + aq + ap, bp + aq)
;T(T(a,b)) = ((bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
;    , (bp + aq)p + (bq + aq + ap)q)
; = ( a (2qq + 2pq + pp) + b (2pq + pp), a (qq + 2pq) + b (pp + qq) )
; => q' = (2pq + qq) , p'=(qq+pp)

(trace fib-iter)
(fib 30)
