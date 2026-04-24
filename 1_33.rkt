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

(define (prime? n)
  (= n (smallest-divisor n)))

; combiners
(define (multiply a b)
  (* a b))

(define (add a b)
  (+ a b))

; predicates
(define (even? n)
  (= (remainder n 2) 0))

(define (odd? n)
  (= (remainder n 2) 1))

; terms
(define (cube n)
  (* n n n))

(define (identity n)
  n)

; nexts
(define (inc n)
  (+ n 1))

(define (inc-odd n)
  (cond ((even? n) (+ n 1))
    (else (+ n 2))))

; filtered accumulator
(define (filtered-accumulate predicate combiner null-value term a next b)
  (cond
    ((> a b) null-value)
    ((predicate a)
     (combiner (term a)
               (filtered-accumulate predicate combiner null-value term (next a) next b)))
    (else
      (filtered-accumulate predicate combiner null-value term (next a) next b))))

(newline)
(display "Sum of prime squares from 1 to 100: ")
(display (filtered-accumulate prime? add 0 square 1 inc 100))
(newline)
(display "Product of prime integers from 1 to 100: ")
(display (filtered-accumulate prime? multiply 1 identity 1 inc-odd 100))
(newline)
