;; I didn't really think about it but when I was writing 1_17 I ended up
;; creating an iterative solution (I was having too much fun with the 
;; primitive operations). I'll instead create the proper recursive version
;; here and leave this comment as record of it instead of renaming the files,
;; mostly because I think it's funny.
(define (double a)
  (+ a a))

(define (even? n)
  (= (remainder n 2) 0))

(define (halve-iter acc n)
  (cond ((= n 0) acc)
        ((> n 0) (halve-iter (+ acc 1) (- n 2)))
        (else    (halve-iter (- acc 1) (+ n 2)))))

(define (halve a)
  (halve-iter 0 a))

(define (mult m n)
  (cond ((= n 0) 0)
        ((even? n) (mult (double m) (halve n)))
        ((> n 0)   (+ m (mult m (- n 1))))
        (else      (- (mult m (+ n 1)) m))))

(display "(mult 10 5): ")
(display (mult 10 5))
(newline)
(display "(mult 47 6): ")
(display (mult 47 6))
(newline)
(display "(mult 367 65536): ")
(display (mult 367 65536))
(newline)
(display "(mult -42 -69): ")
(display (mult -42 -69))
(newline)
