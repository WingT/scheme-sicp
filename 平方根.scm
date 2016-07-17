(define (squareroot x)
  (define (sqrt-iter guess)
    (if (goodenough? guess)
        guess
        (sqrt-iter (improve guess))))
  (define (goodenough? guess)
    (< (/ (abs (- guess (/ x guess))) guess) 0.0000001))
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2))
  (sqrt-iter 1.0))
(display (squareroot 5))