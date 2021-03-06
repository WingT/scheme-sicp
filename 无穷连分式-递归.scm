(define (cont-frac n d k)
  (define (cont-frac-iter i ans)
    (define (step i ans)
      (/ (n i) (+ (d i) ans)))
    (if (= i 0)
        ans
        (cont-frac-iter (- i 1) (step i ans))))
  (cont-frac-iter k 0.0))
(define (n x) 1.0)
(define (d x)
  (define r (remainder x 3))
  (cond ((< r 2) 1.0)
        (else (* 2 (+ 1 (/ (- x r) 3))))))
;; (define (test n)
;;   (define (test-iter count)
;;     (if (> count 1)
;;         (begin
;;           (display (d count))
;;           (display " ")
;;           (test-iter (- count 1)))
;;         (display (d count))))
;;   (test-iter n))
(cont-frac n d 1000)
