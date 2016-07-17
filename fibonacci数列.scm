;;
(define (fib1 n)
  (if (< n 2)
      n
      (+ (fib1 (- n 1))
         (fib1 (- n 2)))))
;;
(define (fib2 n)
  (define (fib2-iter a b n)
    (if (= n 0)
        a
        (fib2-iter b (+ a b) (- n 1))))
  (fib2-iter 0 1 n))
;;
(define (fib3 n)
  (define (square x) (* x x))
  (define (new-p p q)
    (+ (square q) (square p)))
  (define (new-q p q)
    (+ (* 2 p q) (square q)))
  (define (fib3-iter a b p q n)
    (if (= n 0)
        b
        (if (= (remainder n 2) 0)
            (fib3-iter a b (new-p p q) (new-q p q) (/ n 2))
            (fib3-iter (+ (* (+ p q) a) (* q b))
                       (+ (* q a) (* p b))
                       p
                       q
                       (- n 1)))))
  (fib3-iter 1 0 0 1 n))
;;
(define (test fib n)
  (cond ((>= n 0)
         (
          begin
           (display (fib n))
           (display "\n")
           (test fib (- n 1))))))
;;

(test fib3 10)
