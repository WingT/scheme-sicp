(define (D f)
  (define dx 0.00001)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))
((D (lambda (x) (* x x x))) 5)
