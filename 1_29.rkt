#lang sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (even? n)
  (= (remainder n 2) 0))

(define (simptegral f a b n)
  (if (not (even? n))
      0
      (let ((h (/ (- b a) n)))
       (define (apply_coefficient k)
         (cond ((or (= k n) (= k 0)) 1)
           ((even? k) 2)
           (else 4)))
       (define (f_term k)
         (* (apply_coefficient k) (f (+ a (* k h)))))
       (define (f_next k)
         (+ k 1.0))
       (* (/ h 3.0) (sum f_term 0 f_next n)))))

(display "Simpson's Rule(n = 100): ")
(display (simptegral cube 0 1 100))
(newline)

(display "Traditional rule(dx = 0.1): ")
(display (integral cube 0 1 0.1))
(newline)

(display "Simpson's Rule(n = 1000): ")
(display (simptegral cube 0 1 1000))
(newline)

(display "Traditional rule(dx = 0.001): ")
(display (integral cube 0 1 0.001))
(newline)

(display "Simpson's Rule(n = 10000): ")
(display (simptegral cube 0 1 10000))
(newline)

(display "Traditional rule(dx = 0.0001): ")
(display (integral cube 0 1 0.0001))
(newline)
