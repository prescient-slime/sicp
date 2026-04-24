#lang sicp

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

; nexts
(define (inc n)
  (+ n 1))

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
(display "Sum of even cubes from 1 to 5: ")
(display (filtered-accumulate even? add 0 cube 1 inc 5))
(newline)
(display "Sum of odd cubes from 1 to 1000: ")
(display (filtered-accumulate odd? add 0 cube 1 inc 1000))
(newline)
