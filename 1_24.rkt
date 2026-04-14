#lang sicp

(define (square n)
  (* n n))
(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))
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
          (if (fast-prime? flr 10)
              (search-for-prime (+ flr 2) (- n 1))
              (search-for-prime (+ flr 2) n)))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (fast-prime? n times)
  (cond ((= times 0) true)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else false)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
    ((even? exp)
     (remainder (square (expmod base (/ exp 2) m))
                m))
    (else
      (remainder (* base (expmod base (- exp 1) m))
                 m))))

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
