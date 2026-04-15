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
    (= (expmod a (- n 1) n) (remainder 1 n)))
  (try-it (+ 1 (random (- n 1)))))

(define (nontrivial-sqrt? x m)
  (and (= (remainder (square x) m) 1)
       (not (= x 1))
       (not (= x (- m 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
    ((even? exp)
     (let ((prev (expmod base (/ exp 2) m)))
       (if (nontrivial-sqrt? prev m)
           0
           (remainder (square prev) m))))
    (else
      (remainder (* base (expmod base (- exp 1) m))
                 m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (miller-rabin-prime? n times)
  (cond ((= times 0) true)
    ((miller-rabin-test n) (miller-rabin-prime? n (- times 1)))
    (else false)))

; Primes -- both should return true
(newline)
(display "(miller-rabin-prime? 7 10): ")
(display (miller-rabin-prime? 7 10))

(newline)
(display "(miller-rabin-prime? 1009 10): ")
(display (miller-rabin-prime? 1009 10))

; Carmichael numbers -- fermat-test returns true for these,
; miller-rabin should return false
(newline)
(display "(miller-rabin-prime? 561 10): ")
(display (miller-rabin-prime? 561 10))

(newline)
(display "(miller-rabin-prime? 1105 10): ")
(display (miller-rabin-prime? 1105 10))

(newline)
(display "(miller-rabin-prime? 1729 10): ")
(display (miller-rabin-prime? 1729 10))

