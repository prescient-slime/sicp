#lang sicp
; I swapped to rkt because if the maintainers of MIT-Scheme can't be bothered
; to produce an easy-to-use package, then I can't be bothered to use it.

(define (even? n)
  (= (remainder n 2) 0))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* q q) (* p p))
                   (+ (* q (+ p q)) (* p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p 
                        q
                        (- count 1)))))

(newline)
(display "fib 3: ")
(display (fib 3))
(newline)
(display "fib 65535: ")
(display (fib 65535))
(newline)
; I love huge numbers
; My machine's performance graph looks insane
(display "fib 9,223,372,036,854,775,807: ")
(display (fib 9223372036854775807))
(newline)

