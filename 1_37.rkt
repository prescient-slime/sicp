#lang sicp

(define tolerance 0.00001)

(define (average x y)
  (/ (+ x y) 2))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next(try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(define phi (/ (+ 1 (sqrt 5)) 2))

(define (cont-frac n d k)
  (cond ((= k 0) 1.0)
    (else
      (/ n (+ d (cont-frac n d (- k 1)))))))

(define (cont-frac-iter n d k)
  (define (iter fraction counter)
    (cond ((= counter 0) fraction)
      (else (iter (/ n (+ d fraction)) (- counter 1)))))
  (iter (/ n d) k))

(display "1 over phi: ")
(display (/ 1 phi))
(newline)
(display "Continuous fraction with n and d set to 1: ")
(display (cont-frac 1.0 1.0 10))
(newline)
(display "K must equal 10 for 4 decimal place accuracy")
(newline)
(display "Continuous fraction (iterative) with n and d set to 1 (K=1,000,000,000): ")
(display (cont-frac-iter 1.0 1.0 1000000000))
(newline)
(display "Continuous fraction (recursive) with n and d set to 1 (K=100,000,000): ")
(display (cont-frac 1.0 1.0 100000000))
(newline)

