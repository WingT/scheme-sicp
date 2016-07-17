(define (pi term a next b)
  (define (pi-iter a ans)
    (if (> a b)
        ans
        (pi-iter (next a) (* ans (term a)))))
  (pi-iter a 1.0))
(define (fig-pi n)
  (* 4 (pi (lambda (x)
             (/ (* (- x 1) (+ x 1)) (* x x)))
           3
           (lambda (x) (+ x 2))
           n)))
