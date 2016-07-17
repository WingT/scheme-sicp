(define (accumulate combine null-value term a next b)
  (define (accumulate-iter a ans)
    (if (> a b)
        ans
        (accumulate-iter (next a) (combine ans (term a)))))
  (accumulate-iter a null-value))
(define (pi-item a)
  (/ (* (- a 1) (+ a 1)) (* a a)))
(* 4 (accumulate * 1.0 pi-item 3 (lambda (x) (+ x 2)) 10000))
