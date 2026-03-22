(define count 0)
(define (expt b n)
  (expt-iter b n 1))

(define (square n)
  (* n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (expt-iter b n a)
  (set! count (+ count 1))
  (cond ((= n 0) a)
        ((even? n)
         (expt-iter (square b) (/ n 2) a))
        (else
          (expt-iter b (- n 1) (* a b))
  )))

(newline)
(display "(expt 2 16): ")
(display (expt 2 16))
(newline)
(display "count: ")
(display count)
(set! count 0)

(newline)
(display "(expt 3 47): ")
(display (expt 3 47))
(newline)
(display "count: ")
(display count)
(set! count 0)


(newline)
(display "(expt (expt 10 10) (expt 10 10)): ")
(display (expt (expt 10 10) (expt 10 10))) ;; Power towers are fun. Of course, this takes a very, very, very long time.
(newline)
(display "count: ")
(display count)
(set! count 0)
