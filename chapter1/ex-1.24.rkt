#lang racket
(require racket/trace)

(define (runtime) (current-inexact-milliseconds))

(define (square a) (* a a))
(define (divides? a b) (= (remainder b a) 0))

; ====================================================== 
; Fermat test

(define (expmod base exp m)
	(cond ((= exp 0) 1)
		((even? exp)
		  (remainder
		 	(square (expmod base (/ exp 2) m))
		 	m))
		(else
		  (remainder
			(* base (expmod base (- exp 1) m))
			m))))

(define (fermat-test n)
	(define (try-it a)
		(= (expmod a n n) a))
	(try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
	(cond ((= times 0) true)
		((fermat-test n) (fast-prime? n (- times 1)))
		(else false)))

(define (prime? n) (fast-prime? n 100))

; ====================================================== 
; searching for primes

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

; with Fermat test run 100 times we have:
; 5 µs for primes near 1000 and 10 µs near 1000000
; log1000(1000000) = 10/5 = 2