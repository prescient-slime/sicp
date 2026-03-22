(define (f n)
    (if (< n 3)
        n
        (f_iter n 2 1 0 2)
    )
)
(define (f_iter n a b c count)
    (cond
        ((= count n)
         a
        )
        (else 
            (f_iter n (+ a (* 2 b) (* 3 c)) a b (+ count 1))
          )
    )
)

(newline)
(display (f 65536))
(newline)
