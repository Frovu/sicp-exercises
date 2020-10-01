#lang racket
(require racket/trace)

(define (runtime) (current-inexact-milliseconds))

(define (square a) (* a a))
(define (divides? a b) (= (remainder b a) 0))

; added this for ex 1.23
(define (next-divisor d) (if (= d 2) 3 (+ 2 d)))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
		  ((divides? test-divisor n) test-divisor)
		  (else (find-divisor n (next-divisor test-divisor)))))


(define (prime? n)
	(= n (smallest-divisor n)))

(define (timed-prime-test n)
	(do-timed-prime-test n (runtime)))
(define (do-timed-prime-test n start-time)
    (if (do-prime-tests 1000 n)
    	(report-prime n (/ (- (runtime) start-time) 1000)) #f))
    
(define (do-prime-tests num-tests n)
    (cond ((<= num-tests 1) (prime? n))
    	(else (prime? n)
    		 (do-prime-tests (sub1 num-tests) n))))

(define (report-prime n elapsed-time)
	(newline)
    (display n)
    (display " *** ")
    (display (* elapsed-time 100))
    (display " µs"))

(define (search-for-primes range-start range-end amount)
	(if (even? range-start)
		(search-for-primes (add1 range-start) range-end amount)
		(cond ((or (>= range-start range-end) (= amount 0))
			  (display "\ndone"))
			  (else (search-for-primes
			  		(+ 2 range-start)
			  		range-end
			  		(if (timed-prime-test range-start)
			  			(sub1 amount)
			  			amount))))))

(search-for-primes 1000 2000 3)
(search-for-primes 10000 20000 3)
(search-for-primes 100000 200000 3)
(search-for-primes 1000000 2000000 3)
(search-for-primes 10000000 20000000 3)
(search-for-primes 100000000 200000000 3)

; with new version of smallest-divisor we got:
; 0.15 -> 0.07 µs
; 1.4 -> 0.7 µs
; 13.8 -> 7.15 µs
; which is about 2 times faster