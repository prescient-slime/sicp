#lang sicp

(define (f g)
  (newline)
  (display "Called f with argument ")
  (display g)
  (newline)
  (g 2))

(f f)

; The output shows that calling f on itself results in the first call
; being evaluated as a call to f with an argument of the procedure f.
; Then we evaluate the argument of f, which is f with an argument of 2.
; Then we evaluate the call of the argument of f with an argument of 2,
; which results in an error because the procedure tries to evaluate the
; statement (2 2). The sequence could be written as (f f)->(f 2)->(2 2).
