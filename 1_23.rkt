#lang sicp

(define (square n)
  (* n n))
(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0))

(define (next n)
  (cond ((= n 2) 3)
    (else (+ n 2))))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (next test-divisor)))
    ))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime))
  )

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (even? n)
  (= (remainder n 2) 0))

(define (search-for-prime flr n)
  (cond ((= n 0) (newline))
        ((even? flr)
          (search-for-prime (+ flr 1) n))
        (else (timed-prime-test flr)
          (if (prime? flr)
              (search-for-prime (+ flr 2) (- n 1))
              (search-for-prime (+ flr 2) n)))))

(define (prime? n)
  (= n (smallest-divisor n)))

(newline)
(display "(search-for-prime 1000 3): ")
(search-for-prime 1000 3)

(newline)
(display "(search-for-prime 10000 3): ")
(search-for-prime 10000 3)

(newline)
(display "(search-for-prime 100000 3): ")
(search-for-prime 100000 3)

(newline)
(display "(search-for-prime 1000000 3): ")
(search-for-prime 1000000 3)
